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

(IN-PACKAGE :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BULIT-IN CONSTRAINTS FOR OM-SCREAMER -> SCREAMER-SCORE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;=========================================================================;
;===================== MODEL FOR CONSTRAINTS =============================;
;=========================================================================;
#|
(defmethod! CONSTRAINT ((input1 type) (input2 type) (etc. etc.))
 :initvals '(initval1 initval2 etc.)
 :indoc '("input0" "input1" "etc.")
 :doc "DOCUMENTATION"
 :menuins '((0 (("string0" out0) ("string1" out1) ("etc." etc.))) )
 :icon 486 
 (let ((constraint (eval `(lambda (x)
                            "CONSTRAINT" ;<=== THIS IS FOR RETRIEVE THE NAME WHEN GIVING AN ERROR
                           (**BODY** ,(reclist-vars inputs)) ;<=== FOR ALL INPUTS
						   )))
	   )
						    
  ;CONSTRAINT-ONE-VOICE
  (constraint-one-voice constraint
	                    INPUT: "list" "n-inputs" "car-cdr" "growing" 
						VOICES: voices-list
						DOMAIN: "pitch" "pitch-dur" "pitch-onset" "pitch-dur-onset"
						&KEY
						 :percentage-mode "off" "exactly" "less-than" "greater-than" "between"
						 :percent number or list (for "between")
						 :cs-mode | NOT IMPLEMENTED | 
   						)
  ;CONSTRAINT-HARMONY						
  (constraint-harmony constraint
	  	              INPUT: "list" "n-inputs" "car-cdr" "growing"
	  				  VOICE-SELECT: "all-voices" "voices-list"
					  &KEY
					   :voices list ,e.g. '(0 1 2)
					   :domain "pitch" | "pitch-dur", "pitch-onset", "pitch-dur-onset" NOT IMPLEMENTED |
					   :beats "all" "on-beat" "off-beat" "1st-beat" (of measure)
					   :percentage-mode "off" "exactly" "less-than" "greater-than" "between"
					   :percent number or list (for "between")
					   :cs-mode | NOT IMPLEMENTED |  					   
	                  )
  ;CONSTRAINT-PROFILE					  	
  (constraint-profile bpf-object (bpf or bpf-lib)
	  	              VOICES: list
	  				  APPROX: integer (in midi)
					  RANGE: list -> (in midi, e.g. '(60 84)) or string -> "voice-range" or "all"
					  &KEY
					   :scale-time t or nil
					   :cs-mode | NOT IMPLEMENTED |  					   
	                  )						  
  ;CONSTRAINT-MEASURE					  
  (constraint-measure constraint-object (one-voice, harmony or profile)
  					  measure-number or list-of-number				   
	                  )						
					  					
   ))
|#
;=========================================================================;
;																		  ;
;=========================================================================;

;;; MELODIC

(defmethod! allowed-melodic-intervals ((voices list) (intervals list) (mode string))
 :initvals '((0) (3 4 5 7 8 9 12) "abs")
 :indoc '("list" "list" "string")
 :doc "Constraints the selected voices to have only the given intervals."
 :menuins '((2 (("+/-" "+/-") ("abs" "abs"))))
 :icon 486 
 (let ((constraint (eval `(lambda (x y)
                           "ALLOWED-MELODIC-INTERVALS"
			   (if (and (atom x) (atom y)) ;<== NOTES
                                (if (equal "+/-" ,(reclist-vars mode))
                                    (screamer::memberv (s::-v y x) ,(reclist-vars intervals))
                                    (screamer::memberv (om?::absv (s::-v y x)) ,(reclist-vars intervals)))
                               (let* ((intervalsv (if (equal "+/-" ,(reclist-vars mode)) ;<== CHORDS
						      (mapcar #'screamer::-v y x)
                                                      (mapcar #'(lambda (n1 n2) (om?::absv (screamer::-v n2 n1))) x y))))
                                  (all-membersv intervalsv ,(reclist-vars intervals)))
                                )))))		   					
  (constraint-one-voice constraint "n-inputs" voices "pitch")))

  (defun constraint-motifs-internal  (x motifs &optional chain?)
  (let ((vars '())
          (motif-lengths (remove-duplicates (mapcar #'length motifs)))
          curr-len curr-motifs  tests)
  (setf tests (loop for var in x
           do (progn (setf vars (push-end var vars))
                            (when (null curr-len) (progn (setf curr-len (nth-random motif-lengths)) 
                                                                         (setf curr-motifs (loop for motif in motifs
                                                                                                            when (= curr-len (length motif))
                                                                                                            collect motif)))))
          when (= (1- (length vars)) curr-len)
          collect (let ((test (screamer::memberv (om?::list-elements-ofv (x->dxv vars)) curr-motifs)))
                       (setf curr-motifs nil)
                       (setf curr-len nil)
                          (if chain? (setf vars (last vars)) (setf vars nil) )
                       test)))
  (when (not (null vars))
   (loop for m-len in motif-lengths
           do (setf curr-motifs (loop for motif in motifs
                                                   when (= m-len (length motif))
                                                   collect motif)) 
            when (= (1- (length vars)) m-len)
            do (push-end (screamer::memberv (om?::list-elements-ofv (x->dxv vars)) curr-motifs) tests)))
  tests
  ))
  
(defmethod! CONSTRAINT-MOTIFS ((voices list) (motifs list) (chain? om::t))
   :initvals '( (0) ((-3 7) (3 -7) (4 -7) (-4 7)) nil)
   :indoc '("list" "list" "t or nil")
   :doc "CONSTRAINT SELECTED VOICES TO FOLLOW THE INTERVAL MOTIFS (LIST OF LISTS OF INTERVALS)."
   :menuins '((2 (("t" t) ("nil" nil))))
   :icon 486 
   (let ((constraint (eval `(lambda (x)
                              "CONSTRAINT-MOTIFS" 
                             (apply #'screamer::andv 
								 (constraint-motifs-internal x ,(reclist-vars motifs) ,chain?))))))

    (constraint-one-voice constraint "list" voices "pitch")))
							
;;; HARMONIC 

;;; PROFILES

;;; COUNTERPOINT

;=========================================================================;
; BUILT-IN FUNCTIONS (MOVED HERE FROM OM-SCREAMER: CONSTRAINT-UTILS.LISP)	  ;
;=========================================================================;


;(defun flat-chords (x)
; (flat (loop for el in x collect (if (listp el) (reverse el) el))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==> COUNTERPOINT: IN PROGRESS

(defun parallel? (list1 list2)
 (if (or (null (first list1)) (null (first list2))
	     (null (second list1)) (null (second list2)))
		 nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))
  (s::orv (s::andv (s::>v m1 0) ;same direction - same intervals
                   (s::>v m2 0)
	      	       (s::=v m1 m2))
 	      (s::andv (s::<v m1 0)
 	   	           (s::<v m2 0)
				   (s::=v m1 m2))))))

(defun direct? (list1 list2)
(if (or (null (first list1)) (null (first list2))
     (null (second list1)) (null (second list2)))
	 nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))
   (s::orv (s::andv (s::>v m1 0) ;same direction - different intervals
		          (s::>v m2 0)
		          (s::/=v m1 m2))
	     (s::andv (s::<v m1 0)
			   (s::<v m2 0)
		          (s::/=v m1 m2))))))

(defun contrary? (list1 list2)
(if (or (null (first list1)) (null (first list2))
     (null (second list1)) (null (second list2)))
	 nil
(let ((m1 (s::-v (first list2) (first list1)))
      (m2 (s::-v (second list2) (second list1))))
 (s::orv (s::andv (s::<v m1 0) ;opposite directions
			      (s::>v m2 0))
		 (s::andv (s::>v m1 0)
		          (s::<v m2 0))))))

(defun oblique? (list1 list2)
(if (or (null (first list1)) (null (first list2))
     (null (second list1)) (null (second list2)))
	 nil
 (let ((m1 (s::-v (first list2) (first list1)))
       (m2 (s::-v (second list2) (second list1))))
	(s::orv (s::andv (s::=v m1 0)  ;interval 1 = 0 - interval 2 /= 0
			         (s::/=v m2 0))
			(s::andv (s::/=v m1 0);interval 1 /= 0 - interval 2 = 0
				     (s::=v m2 0))))))

(defun stepwise? (n1 n2)
(if (or (null n1) (null n2))
nil
    (s::memberv (om?::absv (s::-v n2 n1)) '(1 2))))

(defun any-step? (list1 list2)
 (s::orv (stepwise? (second list1) (second list2))
         (stepwise? (first list1) (first list2))))

(defun step-upper-voice? (list1 list2)
 (stepwise? (first list1) (first list2)))

(defun tied? (x y)
(if (or (null x) (null y))
nil
    (s::equalv (s::variable-name x) (s::variable-name y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MELODIC-LINE-INTERVALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-asc-desc-lines (notes)
 (let ((all-positions
  (loop for n from 0 to (- (length notes) 2)
        if (> (nth n notes) (nth (1+ n) notes))
        collect (om::x-append n (1+ n)) into descending
        else 
        collect (om::x-append n (1+ n)) into ascending
        finally (return (om::x-append (list (om::flat ascending)) 
                                      (list (om::flat descending)))))))
  (om::posn-match notes (mapcar #'remove-duplicates all-positions))))

(defun split-ascending (notes)
(om::group-list 
  notes 
  (om::x->dx 
  (om::x-append 
   0  
  (loop for n from 0 to (- (length notes) 2)
        if (> (nth n notes) (nth (1+ n) notes))
        collect (1+ n))
  (length notes)))
 'linear))

(defun split-descending (notes)
 (om::group-list 
  notes 
  (om::x->dx 
  (om::x-append 
   0  
  (loop for n from 0 to (- (length notes) 2)
        if (< (nth n notes) (nth (1+ n) notes))
        collect (1+ n))
  (length notes)))
'linear))

(defun get-melodic-intervals (asc-or-desc-line)
 (abs (- (first asc-or-desc-line) (om::last-elem asc-or-desc-line))))
  
(defun melodic-line-intervals (melody allowed-mel-ints)
"This functions returns true if all notes <first input>
in the melodic line contains only allowed intervals
<second input>.
The intervals is calculated from the lowest note to the highest in
a ascending or descending line. 
E.g.: a line '(6000 6400 6600 6200 5900) will return two intervals,
600 (from the ascending line 6000 6400 6600) and 700 (from the
descending line 6600 6200 5900). Then, if those intervals are allowed
the function will return t, otherwise returns nil." 
 (let* ((asc-or-desc-lines (get-asc-desc-lines melody))
       (ascending (split-ascending (first asc-or-desc-lines)))
       (descending (split-descending (second asc-or-desc-lines)))
       (all-intervals (mapcar #'get-melodic-intervals (om::x-append ascending descending))))
 (loop for interval in all-intervals
       always (member interval allowed-mel-ints))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRAINTS

(defmethod! constraint-scale ((mode string) (scale list) (voices-list list))
    :initvals '("midi" (0 2 4 5 7 9 11 12) (0))
    :indoc '( "midi/pc" "list" "list")
    :doc "Constraint one voice to contain only members of the input scale. Returns a screamer-score-constraint object."
	:menuins '((0 (("midi" "midi") ("pc" "pc"))))
    :icon 486

(if (equal mode "pcs")
    (let ((constraint (eval `#'(lambda (x) "constraint-scale-pcs" (if (atom x) 
                                                                                                     (?::hard-memberv  x ,(reclist-vars (m->pcv scale)))
                                                                                        (mapcar #'(lambda (xx) (?::hard-memberv xx ,(reclist-vars (m->pcv scale))))
                                                                                                (flat x)))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))

    (let ((constraint  (eval `#'(lambda (x) "constraint-scale-midi" (if (atom x) 
                                                                                                      (?::hard-memberv (m->pcv  x) ,(reclist-vars (m->pcv scale)))
                                                                                                      (mapcar #'(lambda (xx) (?::hard-memberv (m->pcv xx) ,(reclist-vars (m->pcv scale)))) 
                                                                                                              (flat x)))))))
     (constraint-one-voice constraint  "n-inputs" voices-list "pitch"))))

(defmethod! chords-alldiff ((mode string) (input-mode string) &optional voices-list)
    :initvals '("midi" "all-voices" nil)
    :indoc '("midi/pcs" "all-voices/voices-list" "list")
    :doc "Constraint all chords to contain or only differentc pitch classes or only different midi values. Returns a screamer-score-constraint object."
	:menuins '((0 (("midi" "midi") ("pc" "pc")))
                         (1 (("all-voices" "all-voices") ("voices-list" "voices-list"))))
    :icon 486
    (let ((constraint (if (equal mode "midi")
                                 (eval ` #'(lambda (x) "chords-alldiff" (apply 's::/=v (remove nil (flat x)))))
                                 (eval `#'(lambda (x) "chords-alldiff" (apply 's::/=v  (m->pcv (remove nil (flat x)))))))))
    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices")
       (constraint-harmony constraint  "n-inputs" "voices-list" :voices voices-list))))

(defmethod! no-crossing ((input-mode string) (unison? string) &optional voices-list)
    :initvals '("all-voices" "no" nil)
    :indoc '( "all-voices/voices-list" "yes/no" "list")
    :doc "Returns a screamer-score-constraint object."
    :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list")))
                         (1 (("no" "no") ("yes" "yes"))))
    :icon 486
    (let ((constraint (if (equal unison? "no")
                                 (eval `#'(lambda (x) "no-crossing" 
                                            (let ((vars (remove nil (flat x))))
                                                (if vars 
                                                   (apply #'s::>v vars)
                                                    t))))
                                 (eval `#'(lambda (x) "no-crossing" 
                                            (let ((vars (remove nil (flat x))))
                                                (if vars 
                                                   (apply #'s::>=v vars)
                                                    t)))))))

    (if (equal input-mode "all-voices")
       (constraint-harmony constraint  "n-inputs" "all-voices")
       (constraint-harmony constraint  "n-inputs" "voices-list" :voices voices-list))))

(defmethod! not-parallel-fifths-octaves ((voices-list list))
    :initvals '( ((0 1) (0 2)) )
    :indoc '("list")
    :doc "Returns a screamer-score-constraint object."
    :icon 486
    (let ((constraint
             (eval `#'(lambda (x y) "not-parallel-fifths-octaves"
             (if (or (some #'null x) (some #'null y))
                  t
                  (if (or (some #'listp x) (some #'listp y))
                      (progn (om-message-dialog "The not-parallel-fifths-octaves constraint does not work with voices that contains chords.")
                                  (om-abort))
               (let ((interval1  (s::funcallv #'mod (om?::absv (s::-v (first x) (second x))) 12))
                     (interval2 (s::funcallv #'mod (om?::absv (s::-v (first y) (second y))) 12)))

              (?::ifv (parallel? x y)
                   (s::orv (s::notv (s::memberv interval1 '(0 7)))
                                        (s::notv (s::memberv interval2 '(0 7))))

               t))))))))

   (constraint-harmony constraint  "n-inputs" "voices-list" :voices (if (list-of-listp voices-list) 
                                                                                                          voices-list
                                                                                                         (om?::asc-permutations voices-list 2)))))
(defmethod! not-parallel-intervals ((voices-list list) (intervals list))
    :initvals '( ((0 1) (0 2)) (0 7))
    :indoc '("list" "list")
    :doc "Returns a screamer-score-constraint object."
    :icon 486
    (let ((constraint
             (eval `#'(lambda (x y) "not-parallel-fifths-octaves"
             (if (or (some #'null x) (some #'null y))
                  t
                  (if (or (some #'listp x) (some #'listp y))
                      (progn (om-message-dialog "The not-parallel-fifths-octaves constraint does not work with voices that contains chords.")
                                  (om-abort))
               (let ((interval1  (s::funcallv #'mod (om?::absv (s::-v (first x) (second x))) 12))
                     (interval2 (s::funcallv #'mod (om?::absv (s::-v (first y) (second y))) 12)))

              (?::ifv (parallel? x y)
                   (s::orv (s::notv (s::memberv interval1 ,(reclist-vars intervals)))
                              (s::notv (s::memberv interval2 ,(reclist-vars intervals))))

               t))))))))

   (constraint-harmony constraint  "n-inputs" "voices-list" :voices (if (list-of-listp voices-list) 
                                                                                                          voices-list
                                                                                                         (om?::asc-permutations voices-list 2)))))

(defmethod! chord-at-measure ((measures list) (chords list) (voices list))
  :initvals '( nil nil nil)
  :indoc '("list" "list" "list")
  :doc "Constraint all notes of a voice (or voices) to be members of chord at measure number.

Returns a list of screamer-score-constraint objects."
  :icon 486
   (let ((constraints (loop for chord in chords
                             collect (eval `#'(lambda (x)
                                                "chord-at-measure"
                                                  (all-membersv (list! x) ,(reclist-vars chord)))))))
    (loop for mes in measures
             for cs in constraints
   collect (constraint-measure (constraint-one-voice cs "n-inputs" voices "pitch") mes))))

(defmethod! chord-at-times ((chords list) (onsets list) (voices list) &optional (mode "midi"))
  :initvals '( nil nil nil "midi")
  :indoc '("list" "list" "list" "string")
  :menuins '((3 (("midi" "midi") ("pc" "pc"))))
  :doc "Constraint all notes of a voice (or voices) to be members of chord at the given onset.

The onsets list should be in chronologial order, e.g., a chord (60 64 67) with onset 1/4
will constraint all notes to be members of this chord when onset is greater or equal 0 and smaller
than 1/4.

The number of chords must be the same as the number of onsets.

If the optional <mode> arguments is supplied, it can constraint notes to be members of the pitch class
content of the given chord <pcs> or exactly the same notes of the chord <midi> - default.

Returns a list of screamer-score-constraint objects."
  :icon 486
   (let* ((onset-pairs (mapcar #'list (butlast (x-append 0 onsets)) onsets))
           (constraints (loop for chord in chords
                              for onset-pair in onset-pairs
                              collect  (eval `#'(lambda (x) "chord-at-times"
                                                         (let ((onset (second x)))
                                                        (if (null (first x))
                                                             t
                                                             (if (and (>= onset ,(first onset-pair))
                                                                         (< onset ,(second onset-pair)))
                                                                 (if (equal ,mode "midi")
                                                                     (all-membersv (list! (first x)) ,(reclist-vars chord))
                                                                     (all-membersv (m->pcv (list! (first x)))  ,(reclist-vars (remove-duplicates (m->pcv chord)))))
                                                             t))))))))
    (loop for cs in constraints
             collect (constraint-one-voice cs "n-inputs" voices "pitch-onset"))))

(defmethod! symmetrical-chords? ((input-mode string) &key (voices '(0 1 2)))
  :initvals '("all-voices" (0 1 2))
  :indoc '("string" "list")
  :doc "Constraint all chords to be symmetrical."
  :icon 486
  :menuins '((0 (("all-voices" "all-voices") ("voices-list" "voices-list"))))
 (let* ((cs (eval `#'(lambda (x) "symmetrical-chords?"
                      (let* ((flat-list (flat x))
							 (intervalsv (x->dx-absv (remove nil flat-list)))
							 (len (length intervalsv))
							 (positions (arithm-ser 0 (1- len) 1))
							 (symm-posn (cond ((oddp len)
								               (if (= 1 len)
											        0
												   (mat-trans (list (first-n positions (/ (1- len) 2))
												   	                (reverse (last-n positions (/ (1- len) 2)))))))
												  (t (if (= 2 len)
												         '(0 1)
														 (mat-trans (list (first-n positions (/ len 2))
														   	                 (reverse positions (/ len 2)))))))))
					   (if (listp symm-posn)
						   (apply #'s::andv (mapcar #'(lambda (x)
						  	            (s::=v (first x) (second x)))
					            (posn-match intervalsv symm-posn)))
                                                         t))))))
 (constraint-harmony cs "n-inputs" input-mode :voices voices)))
 
(defmethod! mel-line-intervals ((intervals list) (voices-list list))
 :initvals '((0 2 3 4 5 7 8 9) (0))
 :indoc '("list" "list")
 :doc "This functions returns true if all notes <first input>
in the melodic line contains only allowed intervals
<second input>.
This constraint will be individually applied to each voice listed
in the <third input>.
The intervals is calculated from the lowest note to the highest in
a ascending or descending line. 
E.g.: a line '(6000 6400 6600 6200 5900) will return two intervals,
600 (from the ascending line 6000 6400 6600) and 700 (from the
descending line 6600 6200 5900). Then, if those intervals are allowed
the function will return t, otherwise returns nil."
 :icon 486
 (let ((constraint (eval `(lambda (x) "melodic-line-intervals"
            (om?::any-fn #'(lambda (vars)
			                (if (= 1 (length vars))
							     t
								 (melodic-line-intervals vars ,(reclist-vars intervals))))
					        x)))))
  (constraint-one-voice constraint "growing" voices-list "pitch")))

(defmethod! no-octaves ((voices list))
 :initvals '((0 1))
 :indoc '("list")
 :doc "Constraint all selected voices to not have octaves (unisons are allowed)."
 :icon 486
 (let ((constraint (eval `(lambda (x) 
                                     "no-octaves"
                                     (no-oct x)))))
  (constraint-harmony constraint "n-inputs" "voices-list" :voices voices)))

(defmethod! NOT-DIRECT-FIFTHS-OCTAVES ((voices list))
 :initvals '((0 1))
 :indoc '("list or list-of-lists")
 :doc "Constraints selected voices to not form direct fifths or octaves"
 :icon 486 
 (let ((constraint (eval `(lambda (x y)
                            "NOT DIRECT FIFTHS AND OCTAVES"
							(?::ifv (direct? x y)
							        (screamer::notv
								     (screamer::memberv
									  (modv (om-absv (screamer::-v (first y) (second y)))
									         12)
								      '(0 7)))
									t)))))
					
  (constraint-harmony constraint
	  	              "n-inputs"
	  				  "voices-list"
					   :voices voices)))
