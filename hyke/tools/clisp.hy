;;; Common lisp forms 
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
(import [hy.models.symbol [HySymbol]])

(defun symbol? [x] 
  (isinstance x HySymbol))

(defun atom? [x] 
  (if (coll? x) (= x ()) True))

(defmacro cad?r [seq n]
   `(try 
      (get seq ~n)
      (catch [IndexError] ())))

(defun cadr [seq] (cad?r seq 1))

(defun caddr [seq] (cad?r seq 2))

(defun nconc [seq1 seq2] 
  (.extend seq1 seq2) seq1)



