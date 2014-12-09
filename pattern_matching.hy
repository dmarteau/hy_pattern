
;; Convert the argument list to a cons list
;; XXX does not work with 'apply'
(defmacro listq [&rest seq] `(list* ~@seq '()))


;; Convert the input sequence to a cons list 
;; Note that following modify its input value
(defun list# [seq]
  (.append seq '())
  (apply list* seq))


(defun mapcar [fun x &rest seqs]
  ;; Very much like zipwith except it returns a cons list
  (list# (list-comp
    (apply fun y) 
    (y (apply zip (cons x seqs))))))


(import [hy.models.symbol [HySymbol]])

(defmacro symbol? [x] `(isinstance ~x HySymbol))
(defmacro atom? [x] `(if (coll? ~x) (= ~x ()) True))


(defun union [&rest args]
  (import [itertools [chain]])
  (set-comp x [x (apply chain args)]))  


(defun var? (x)
  ;; Check for variable symbol if it begin with ?
  (and (symbol? x) (= (get (name x) 0) "?")))


(defun match? [expr val &optional [vars {}]]
  (if (atom? expr)
      (if (var? expr)
          (if (in expr vars) ;; Check if vars is already defined
              (= (get vars expr) val)
              (do 
                (assoc vars expr val)
                True))
          (= expr val)) ;; Compare actual values
      ;; Not an atom, go recursively 
      (if (or (atom? val) (!= (len expr) (len val)))
          False
          (all (zipwith (fn [p v] (match? p v vars)) expr val))))) 
          ;;(and (match? (car expr) (car val) vars)
          ;;     (match? (cdr expr) (cdr val) vars))))))


(defun vars-in [expr]
  ;; Returns all the pattern variables in an expression
  ;; It calls var? to test if something is a variable.
  (if (atom? expr)
    (if (var? expr) [expr] [])
    (union (vars-in (car expr))
           (vars-in (cdr expr)))))


(defmacro eval-pattern [expr]
  `(let ~(mapcar (lambda (v) [v `'~v]) (vars-in expr)) ~expr))
         

(defmacro match-if [pat seq then &optional [else nil]]
   (setv if-expr `(if (match? (eval-pattern ~pat) ~seq vars)
       (let ~(mapcar (lambda (v) [v `(get vars '~v)]) (vars-in pat)) ~then)))
   (if (!= else nil) 
       (.append if-expr else)) 
  `(let [[vars {}]] ~if-expr))


(defmacro import-pattern-fns []
  `(import [pattern_matching [*]]))

