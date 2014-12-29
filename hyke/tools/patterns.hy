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
;;  (if-match ['?a '?b 1 "foo"] ["v1" "v2" 1 "bar"]
;;            true-branch ; variables ?a and ?b are binded to their counterpart values in rhs
;;            false-branch)

;; Convert the argument list to a cons list
(defmacro listq [&rest seq] `(list* ~@seq '()))


(defun list# [seq] 
  "Convert the input sequence to a cons list, 
   note that input is modified"
  (.append seq '())
  (apply list* seq))


(defun mapcar [fun x &rest seqs]
  "Very much like zipwith except it returns a cons list"
  (list# (list-comp
    (apply fun y) 
    (y (apply zip (cons x seqs))))))


(import [hy.models.symbol [HySymbol]])

(defmacro symbol? [x] `(isinstance ~x HySymbol))
(defmacro atom? [x] `(if (coll? ~x) (= ~x ()) True))


(defun union [&rest args]
  (import [itertools [chain]])
  (set-comp x [x (apply chain args)]))  


(defun var? [x]
  "Check for variable symbol if it begin with ?"
  (and (symbol? x) (= (get (name x) 0) "?")))


(defun keyword? [x]
  "Check symbol is keyword"
  (and (symbol? x) (= (get (name x) 0) "&")))


(defun match? [expr val &optional [vars {}]]
  "Compare recursively expr and val term by term.
   Variables forms will be binded to right hand side values.
   An optional dictionary can be passed to collect 
   values associated to binded variables"
  (if (atom? expr)
      (if (var? expr)
          (if (in expr vars) ;; Check if vars is already defined
              (= (get vars expr) val)
              (do 
                (assoc vars expr val)
                true))
          (= expr val)) ;; Compare actual values
      ;; Not an atom, go recursively 
      (let [[elem (car expr)]]
        (if (and (symbol? elem) (= elem '&rest))
            (match? (car (cdr expr)) val vars)
            (and (match? (car expr) (car val) vars)
                 (match? (cdr expr) (cdr val) vars))))))


(defun vars-in [expr &optional [varform? var?]]
  "Returns all the pattern variables in an expression
   It calls var? to test if something is a variable."
  (if (atom? expr)
    (if (varform? expr) [expr] [])
    (union (vars-in (car expr) varform?)
           (vars-in (cdr expr) varform?))))


(defmacro/g! if-match* [pat seq then &optional [else nil]]
  "Compare pat and seq and perform destructured assignement with
   variables begining with ? in pat."
   ;;        (if-match [['?a '?b] 1 2] [['foo 'bar] 1 2]
   ;;              (assert (and (= ?a 'foo) (= ?b 'bar)))
   ;;              (assert False)))
  (setv if-expr `(if (match?  ~pat ~seq ~g!vars)
      (let ~(mapcar (lambda (v) [v `(get ~g!vars '~v)]) (vars-in pat)) ~then)))
  (if (!= else nil) 
      (.append if-expr else))

  `(let [[~g!vars {}]] ~if-expr))


(defmacro if-match [&rest args]
  `(do 
     (import [hyke.tools.patterns [match?]])
     (if-match* ~@args)))


(defmacro match-cond* [expr &rest branches]
  "cond with pattern matching
         (match-cond expr 
               [pat1 branch1]
               [pat2 branch2]
               ...
               ['?_ all-match-branch])
   Equivalent to: 
         (if-match pat1 expr 
             branch1 
             (if-match pat2 expr 
                 branch2
                 ...
                 (if-match '?_ all-match-branch)...))"
  (setv branches (iter branches))
  (defun make-branch [branch]
    (setv (, pat thebranch) branch)
    `(if-match* ~pat expr ~thebranch))

  (setv root (make-branch (next branches)))
  (setv last-branch root)
  (for* [branch branches]
    (setv cur-branch (make-branch branch))
    (.append last-branch cur-branch)
    (setv last-branch cur-branch))
 
  `(let [[expr ~expr]] ~root))


(defmacro match-cond [&rest args]
  `(do 
     (import [hyke.tools.patterns [match?]])
     (match-cond* ~@args)))

