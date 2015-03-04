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
;; Plan:
;;
;;   (defrule rulename
;;        [(:notany pattern)
;;         (:match pattern :when clause)
;;         (:match pattern :in statement :when clause)
;;         (:do statement)]   
;;       conclusion)
;;

(require hyke.tools.patterns)

(defun pattern-premise-notany-1 [statement pat premises conclusion getfacts &optional [when-clause nil]]
   "Generate code for handling notany premises"
   ; Notany premise check all facts in statement for falseness
   ; all bindings that occurs during evaluation are undone
  `(for [fact ~statement]
       (if-match ~pat fact
         ~(if (!= nil when-clause) 
            `(if ~when-clause (break))
            `(break)))
     (else ~(foreach-premise premises conclusion getfacts))))


(defun pattern-premise-1 [statement pat premises conclusion getfacts &optional [when-clause nil]]
  "Generate code for matching premise"
  ; loop over all facts and if fact match, handle next premise
  `(for [fact ~statement]
       (if-match ~pat fact
         ~(if (!= nil when-clause)
            `(if ~when-clause ~(foreach-premise premises conclusion getfacts))
             (foreach-premise premises conclusion getfacts)))))


(defun statement-premise [statement premises conclusion getfacts]
  ; Execute statement (which is always true)
  `(do
    ~@statement ; statement is always true
    ~(foreach-premise (premises conclusion getfacts))))


(defun forall-premise [nested next-premises conclusion getfacts &optional [require-clause nil]] 
  "Backtrack all nested premises in the forall premise"
  (setv clause-expr (if (!= nil require-clause) `(if ~require-clause) `()))
  (.apppend clause-expr (foreach premise next-premises conclusion getfacts))
  `~(foreach-premise nested clause-expr getfacts))


(defun match-pattern-premise [fun pat rec next-premises conclusion getfacts]
  (print "xxxx" rec)
  (match-cond rec
     [[]  (fun `(~getfacts pat) pat next-premises conclusion getfacts)]
     [[':when ?clause] (fun (getfacts pat) pat next-premises conclusion getfacts ?clause)]
     [[':in ?statement] (fun ?statement pat next-premises conclusion getfacts)]
     [[':in ?statement ':when ?clause] (fun ?statement pat next-premises conclusion getfacts ?clause)]
     [?_
        (macro-error rec "Matching premise arguments must be [:in statement] [:when clause]")]))
   

(defun foreach-premise [premises conclusion getfacts] 
  (if (= () premises) ; end of cycle, all premises are verified
    conclusion
    (do 
      (setv (, premise next-premises) [(car premises) (cdr premises)])
      (setv (, r rec) [(car premise) (cdr premise)])
      (cond [(= r ':match)
             (match-pattern-premise pattern-premise-1 (car rec) (cdr rec) next-premises conclusion getfacts)]
            [(= r ':notany) 
             (match-pattern-premise pattern-premise-notany-1 (car rec) (cdr rec) next-premises conclusion getfacts)]
            [(= r ':do)
             (statement-premise rec next-premises conclusion getfacts)]
            [(= r ':forall)
             (forall-premise rec next-premises conclusion vars getfacts)]
            [true 
             (macro-error rec "Matching premise arguments must be [:match|:notany|:do|:forall]")]))))
               


(defmacro genrule [premises conclusion getfacts] 
  (foreach-premise premises conclusion getfacts))


;; (defmacro defrule [rulename premises conclusion]
;;  "Define a rule object"
;;  
;; )

