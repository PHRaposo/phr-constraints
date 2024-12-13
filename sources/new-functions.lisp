;; NEW FUNCTIONS 
 ;; =================== ;
;; SCREAMER
(in-package :screamer)

(defun oddpv (x)
 (funcallv #'oddp x))
 
 (defun evenpv (x)
 (funcallv #'evenp x))
 
 ;; =================== ;
 ;; SCREAMER-PLUS
 (in-package :screamer+)
 
 (screamer::defmacro-compile-time condv (&rest clauses)
  "Equivalent of COND macro for Screamer."
  (if (null clauses)
      nil
      (let* ((clause (first clauses))
             (test (first clause))
             (consequent (second clause))
             (alternate (if (null (rest clauses))
                            nil
                            `(condv ,@(rest clauses)))))
        `(ifv ,test ,consequent ,alternate))))

 ;; =================== ;
 ;; OM  
 
(in-package :om)

(defmethod! lists=v ((l1 list) (l2 list))
:initvals '(nil nil) :indoc '("list" "list")
:icon 476
(om?::lists=v l1 l2))

