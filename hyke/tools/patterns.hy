;;; Pattern matching syntax 
;;
;; Copyright (c) 2014  David Marteau <dhmarteau@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;
;; These macro allows writing destructured pattern matching with variable binding 
;;
;; ex:
;;  (if-match [?a ?b 1 "foo"] ["v1" "v2" 1 "bar"]
;;            true-branch ; variables a and b are binded to their counterpart values in rhs
;;            false-branch)

(import [hyke.tools.clisp [*]]
        [hy [HyInteger]]
        [hy.models.symbol [HySymbol]])


(defun var? [x]
  "Check for variable symbol if it begin with ?"
  (and (symbol? x) (= (get (name x) 0) "?")))


(defun var-to-symbol [v]
  (if (= (get (name v) 0) "?") 
    (HySymbol (slice v 1))
    v))


(defun wrap-int [x] (HyInteger x)) ; Fix https://github.com/hylang/hy/issues/707


(defun destruct-match [pat seq vars &optional [n 0]]
  ;; Create a 'let' expression by destructuring 'pat' and binding
  ;; variable to their equivalent place in the input sequence 'seq' 
  ;; Bindings are done with the postfixed value of the declared variable (i,e ?foo binds foo)
  (defun bind-var [elem place]
    (if (and (var? elem) (not (in elem vars)))
      (do
        (.add vars elem)
        `[~(var-to-symbol elem) ~place])
      `[~(gensym) (if (!= ~(var-to-symbol elem) ~place) (raise (IndexError "No match")))]))
  
  (if (= () pat)
    ()
    (let [[elem (cond [(atom? pat) pat]
                      [(= (car pat) '&rest) (cadr pat)]
                      [true None])]]
      (if-not (nil? elem) 
         (if (= elem "?_") `[] `[~(bind-var elem `(slice ~seq ~(wrap-int n)))])
         (do
           (setv (, p rec) [(car pat) (lambda [] (destruct-match (cdr pat) seq vars (inc n)))])
           (if (atom? p)
             (if (= p "?_") (rec) (nconc [`~(bind-var p `(get ~seq ~(wrap-int n)))] (rec)))
             (let [[var (gensym)]]
               (nconc (nconc `[[~var (get ~seq ~(wrap-int n))]]
                   (destruct-match p var vars)) (rec)))))))))
                               

(defmacro/g! if-match [pat seq then &optional [else nil]]
  "Compare pat and seq and perform destructured assignement with
   variables begining with ? in pat."
   ;;        (if-match [[?a ?b] 1 2] [['foo 'bar] 1 2]
   ;;              (assert (and (= ?a 'foo) (= ?b 'bar)))
   ;;              (assert False)))
   `(let [[~g!seq ~seq]]
     (try
       (let  ~(destruct-match pat g!seq (set)) ~then)
       (catch [IndexError] ~(if (= else nil) `nil else)))))


(defmacro match-cond [expr &rest branches]
  "cond with pattern matching
         (match-cond expr 
               [pat1 branch1]
               [pat2 branch2]
               ...
               [?_ all-match-branch])
   Equivalent to: 
         (if-match pat1 expr 
             branch1 
             (if-match pat2 expr 
                 branch2
                 ...
                 (if-match ?_ all-match-branch)...))"
  (setv branches (iter branches))
  (defun make-branch [branch]
    (setv (, pat thebranch) branch)
    `(if-match ~pat expr ~thebranch))

  (setv root (make-branch (next branches)))
  (setv last-branch root)
  (for* [branch branches]
    (setv cur-branch (make-branch branch))
    (.append last-branch cur-branch)
    (setv last-branch cur-branch))
 
  `(let [[expr ~expr]] ~root))


