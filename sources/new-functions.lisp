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
 
(defun any-integerpv (x &key (full-propagation nil))
 "This function returns true if a number is an integer. Works with integers, floats or rational numbers. 
 If x is an unbound variable at function invocation and full-propagation is t, this function will also remove any 
 non-integer number from the variable enumerated domain."
(if (bound? x)
    (zerop (mod x 1))
     (let ((z (a-booleanv)))
      (assert! (eqv z (funcallv #'zerop (funcallv #'mod x 1))))
      (attach-noticer!
       #'(lambda()
	   (when (and full-propagation (not (bound? x))(enumerated-domain-p x) (not (bound? z)))
	    (assert!-memberv-internal
		 x
		(remove-if #'(lambda(e) (plusp (mod e 1)))
			   (variable-enumerated-domain x)))))
	    x)
    z)))

 ;; =================== ;
 ;; OM  
 
(in-package :om)

(defmethod! lists=v ((l1 list) (l2 list))
:initvals '(nil nil) :indoc '("list" "list")
:icon 476
(om?::lists=v l1 l2))

(defmethod! max-harmonic-interval ((max-int number))
 :initvals '(12)
 :indoc '("number")
 :doc "Constraint all voices to the maximum harmonic interval."
 :icon 486
 (let ((constraint (eval `(lambda (input interval) 
                            "max-harmonic-interval"
                           (let ((variables (remove nil (flat input))))
                            (screamer:<=v (screamer:-v (last-elem variables)
						       (first variables))
		                         ,(reclist-vars max-int)))))))
  (constraint-harmony constraint "n-inputs" "all-voices")))
