;;; if/cond expressions with regex
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
;; These macro allows writing if/cond expression with regex
;; The reader macro #\n allows accessing captured groups in
;; the context of the test.
;; ex:
;;  (re-search r"^(\d+)" "1234 foo"
;;            (print #\1 "matched")
;;            (print "no match")

(import re)

(defreader \ [n] 
  (cond [(= '@ n)  `(.groups __sre_match__)]
        [(and (integer? n) (zero? n)) `(.group __sre_match__)]
        [True `(get (.groups __sre_match__) (dec ~n))]))


;; XXX Not sure that's a good idea to define this at global level
;;(defreader s [expr] `(re.compile ~expr))


(defmacro re-search* [rexpr test then-branch &optional (else-branch nil)] 
  """Test the regular expression and execute the branch according to the 
     result of the test:
         (re-search r'^([/d]+)' '1234'
                (print 'found' #\1)
                (print 'Not Found')))
  """
  (setv if-expr `(if-not (none? __sre_match__) ~then-branch))
  (if (!= else-branch nil)
    (.append if-expr else-branch))
  ; if string is litteral, call re.search
  ; else we assume that it is a compiled regular expression
  (if (string? rexpr) 
     `(let [[__sre_match__ (.search re ~rexpr ~test)]] ~if-expr)
     `(let [[__sre_match__ (.search ~rexpr ~test)]] ~if-expr)))           


(defmacro re-search [&rest args]
  ; Version of the macro that import required modules
  `(do
     (import re)
     (re-search* ~@args)))


(defmacro re-cond* [test &rest branches]
  """Nested re-search 
           (re-cond test
                 [regex1 branch1]
                 [regex2 branch2]
                 [".*" all-match-branch])
     is equivalent to: 
         (re-search regex1 expr 
             branch1 
             (re-search regex2 expr 
                branch2
                ...
                (re-search r".*"  all-match-branch)...))
  """
  (setv branches (iter branches))
  (defun make-branch [branch]
    (setv (, pat thebranch) branch)
    `(re-search* .search ~pat _expr ~thebranch))

    (setv root (make-branch (next branches)))
    (setv last-branch root)
    (for* [branch branches]
      (setv cur-branch (make-branch branch))
      (.append last-branch cur-branch)
      (setv last-branch cur-branch))

   `(let [[_expr ~expr]] ~root))



(defmacro re-cond [&rest args]
  ; Version of the macro that import required modules
  `(do
     (import re)
     (re-cond* ~@args)))


