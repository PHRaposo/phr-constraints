;; -------------------- ;;
;; FIX 19/11/2024
;; -------------------- ;;

(in-package :om)

(defun compile-screamer-constraint (fun) ;;;CE
(handler-bind ((error #'(lambda (c)
                       (when *msg-error-label-on*
                         (om-message-dialog (string+ "Error while evaluating the function " "compile-screamer-constraint" " : "
                                                  (om-report-condition c))
                                            :size (om-make-point 300 200))
                         (om-abort)))))
 (let* ((expr (function-lambda-expression fun))
        (patchbox (find-lambda-patchbox fun))
        (patch-name (if (stringp patchbox);<== lambda function documentation
		                 patchbox 
						 (if (null patchbox);<== function in lambda mode
						     (symbol-name (second (cadr (third expr))))
							 (name (reference patchbox))))));<== patch in lambda mode
    (if (compiled-function-p fun) ;expr)
        expr
    (compile (eval `(defun ,(gensym (if (null patch-name) "anon-fun-" (concatenate 'string patch-name "-"))) ,(function-lambda-list fun)
		             (apply ,fun (list ,.(function-lambda-list fun))))))
	;(compile (make-anon-screamerfun expr patch-name))
	))))
