;; Copyright (c) 2024 Paulo Henrique Raposo

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;==========================;
;  UTILS - LISP FUNCTIONS  ;
;==========================;

;(in-package :phr-constraints)

;(defmethod! <NAME> ( )
; :initvals '( )
; :indoc '( )
; :doc ""
; :menuins '((0 (("X" "X") ("Y" "Y")))
;	         (1 (("X" "X") ("Y" "Y")))
;            )
; :icon 486 

;;=================================;;
;; SCREAMER NEW FUNCTIONS

(in-package :om-screamer)

(defun all-notv-memberv (e sequence)
 (let ((sequence-flat (om::flat sequence)))
  (cond ((listp e)
         (apply #'andv (mapcar #'(lambda (x) (notv (memberv x sequence-flat))) (om::flat e))))
         (t (notv (memberv e sequence-flat))))))

;;=================================;;

(in-package :om)

(defmethod! get-fn ((fn-name symbol))
 :initvals '( > )
 :indoc '( "symbol")
 :doc "Returns the function from a given symbol."
 :icon 486 
 (eval `(function ,fn-name)))

(defmethod! get-fn ((exp list))
  :initvals '( '(lambda (x) (declare (ignore x)) nil) )
  :indoc '( "lisp expression")
  :doc "Returns the function from a given LISP expression."
  :icon 486 
  (eval `(function ,exp)))

(defmethod! collect-constraints ((select t) &rest cs)
  :initvals '(:all nil)
  :indoc '("symbol or list" "constraints")
  :doc "Collects all screamer score constraints or a selection of constraint (list of positions)."
  :icon 486 
  (cond ((equal select :all)
         (remove nil cs))
        ((listp select)
         (remove nil (posn-match cs select)))
        (t (om-message-dialog "SELECT MUST BE THE SYMBOL :ALL OR A LIST OF POSITIONS")
           (om-abort))))

(defmethod! all-true? ((boolean-vars list))
  :initvals '(nil)
  :indoc '("list")
  :doc "This function returns a screamer boolean variable constrained to be the result of the test (APPLY 'SCREAMER::ANDV <LIST>)."
  :icon 486 
  (apply #'screamer::andv boolean-vars))

(defmethod! all-notv-memberv ((vars list) (sequence list))
  :initvals '(nil nil)
  :indoc '("list" "list")
  :doc "This function returns a screamer boolean variable constrained to be the result of the test (APPLY 'SCREAMER::ANDV <LIST>)."
  :icon 486 
  (om?::all-notv-memberv vars sequence))

(defmethod! list-memberv? ((l1 list) (l2 list))
:initvals '(nil nil) :indoc '("list" "list")
:icon 476
(screamer::memberv (om?::list-elements-ofv l1) l2))

(defun no-oct (vars)
"Constraint all variables to not form octaves (unisons are allowed)."
 (let ((variables (variables-in vars)))
  (if (= 1 (length variables))
      t
    (all-notv-memberv (all-intervalsv variables) (remove 0 (arithm-ser -120 120 12))))))
	
(defmethod! list-maxv ((x list))
:initvals '(nil)
:indoc '("list")
:doc "Returns a SCREAMER variable constrained to be the maximun value of a list of variables."
:icon 480
(apply #'screamer::maxv x))

	
(defmethod! list-minv ((x list))
:initvals '(nil)
:indoc '("list")
:doc "Returns a SCREAMER variable constrained to be the minimun value of a list of variables."
:icon 480
(apply #'screamer::minv x))

(defmethod! all-ascendingv ((x list))
:initvals '(nil)
:indoc '("list")
:doc "TODO"
:icon 474
(apply #'screamer::<v x))

(defmethod! all-descendingv ((x list))
:initvals '(nil)
:indoc '("list")
:doc "TODO"
:icon 474
(apply #'screamer::<v x))

(defmethod! om-mod ((n number) (d number))
:initvals '(-3 12)
:indoc '("number or list" "number")
:doc "Returns N (number or list of numbers) modulo D."
:icon 209
(mod n d))

(defmethod* om-mod ((n list) (d number))
(mapcar #'(lambda (x)
	(mod x d)) n))

(defmethod! om-rem ((n number) (d number))
:initvals '(-3 12)
:indoc '("number or list" "number")
:doc "Returns N (number or list of numbers) remainder of division by D."
:icon 209
(rem n d))

(defmethod* om-rem ((n list) (d number))
(mapcar #'(lambda (x)
	(rem x d)) n))

(defmethod! mk-poly ((tempo number) (time-sigs list) &rest rtm)
:initvals '(60 (4 4) (1/4 1/4 1/4 1/4))
:indoc '("number" "list" "list")
:doc "Returns a POLY object from tempo and time signature (or list of time signatures) with N voices, one for each list of ratios/trees or voice objects."
:icon 486
(let ((voices (loop for r in rtm
                            when r
                            collect (cond ((voice-p r) r)
                                                  ((and (listp r) (every #'numberp r))
                                                   (make-instance 'voice :tree (reduce-rt (mktree r time-sigs)) :tempo tempo))
                                                  (t (make-instance 'voice :tree r :tempo tempo))))))
(make-instance 'poly :voices voices)))

(defmethod! simple->poly ((tempo number)(time-sigs list) (ratios list) (chord-seq t) (voices list))
:initvals '(60 (4 4) ((1/4 1/4 1/4 1/4)) ((6000 6000 6000 6000)) (0))
:indoc '("list" "list" "chord-seq or midics" "list" "list")
:doc "Returns a POLY object from a time signature (or list of time signatures) with N voices, one for each list of ratios."
:icon 486
(let* ((chords chord-seq)
       (voices (loop for r in ratios 
                            for x from 0
                            if (member x voices)
                            collect (make-instance 'voice :tree (reduce-rt (mktree r time-sigs)) :chords (pop chords) :tempo tempo)
                            else
                           collect (make-instance 'voice :tree (reduce-rt (mktree r time-sigs)) :tempo tempo))))
(make-instance 'poly :voices voices)))

;;;RHYTHMIC CONSTRUCTOR

(defmethod! cons-tree ((timesig list) (puls list) (subdiv list))
  :initvals '( ( (5 8) (6 8) (6 8)) ((2 3) (2 2 2) (1)) (((1 1) (1 1 1)) ((1 1) (1 1) (1 1)) ((1.0))))
  :indoc '( "list" "list" "list")
  :doc
"Constructs a rhythmic tree from three arguments:
(1) A list of time signatures;
(2) A list of lists of pulses subdivisions;
(3) A list of lists of beats subdivisions.
"
  :icon 254
(list '?
      (mapcar #'(lambda (tim p s)
                 (list tim
                      (mapcar #'list p s)))
       timesig puls subdiv)))

