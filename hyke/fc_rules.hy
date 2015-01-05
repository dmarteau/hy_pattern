;;; Forward chaining rules macros
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
i
;; Plan:
;;
;;   (defrule rulename
;;        [(:notany pattern)
;;         (:match pattern :when clause)
;;         (:match pattern :in statement :when clause)
;;         (:do statement)]   
;;       conclusion)
;;

(import [hyke.tools.patterns [destruct-match]])


(defun pattern-premise-notany-1 [statement pat premises conclusion vars n [&optional [when-clause nil]]]
  `(for [fact statement]
     (try
       (setv vars-1 (set vars))
       (let ~(destruct-match pat fact vars-1)
         ~(if (!= nil when-clause) 
            `(if ~when-clause (break))
            `(break)))
       (catch IndexError]))
     (else ~(foreach-premise premises conclusion n))))


(defun pattern-premise-1 [statement pat premises conclusion vars n [&optional [when-clause nil]]]
  `(for [fact statement]
     (try
       (let ~(destruct-match pat fact vars)
         ~(if (!= nil when-clause)
            `(if ~when-clause ~(foreach-premise premises conclusion n))
            ~(foreach-premise premises conclusion n)))
       (catch IndexError]))))


(defun statement-premise [statement premises conclusion vars n]
  (do
    ~@statement ; statement is always true
    (foreach-premise (premises conclusion vars n))))


(defun match-pattern-premise [fun pat rec next-premises conclusion vars n]
   (match-cond rec
     [[()]  (fun `(get-facts ~n) ?pat next-premises conclusion vars (inc n))]
     [[':when ?clause] (fun `(get-facts ~n) ?pat next-premises conclusion vars (inc n) ?clause)]
     [[':in ?statement] (fun ?statement ?pat next-premises conclusion vars n)]
     [[':in ?statement ':when ?clause] (fun ?statement ?pat next-premises conclusion vars n ?clause)]
     [[?_] 
        (macro-error rec "Matching premise arguments must be [:in statement] [:when clause]]")]))


(defun forall-premise [])
   

(defun foreach-premise [premises conclusion vars n ]
   (if (= () patterns) ; end of cycle, all premises are verified
     conclusion
     (do 
       (setv (, premise next-premises) [(car premises) (cdr premises))
       (match-cond (car premise) 
         [[':match ?pat &rest ?rest]  
          (match-pattern-premise pattern-premise-1 ?pat ?rest next-premises conclusion vars n)]
         [[':notany ?pat &rest ?rest] 
          (match-pattern-premise pattern-premise-notany-1 ?pat ?rest next-premises conclusion vars n)]
         [[':do &rest ?statement] 
          (statement-premise ?statement next-premises conclusion vars n)]
         [[':forall ?nested]
          (forall-premise ?nested next-premises conclusion vars n)
         [[?_]
          (macro-error rec "Matching premise arguments must be [:match|:notany|:do|:forall]")]))))

    


(defmacro defrule [rulename premises conclusion]
  )

