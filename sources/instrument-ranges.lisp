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

(in-package :phr-constraints)

(defun make-instrument-range (name &optional (approx 1))
 (let ((low-high (eval name)))
  (om::arithm-ser (first low-high) (second low-high) approx)))
  
(defvar *instrument-ranges-hash* (make-hash-table :test #'equal))

(defun fill-instruments-hash ()
 (let ((instruments-list *all-instrument-ranges*))  
      (clrhash *instrument-ranges-hash*)
      (dolist (inst instruments-list) 
        (setf (gethash inst *instrument-ranges-hash*)  
              (make-instrument-range inst)))))

(fill-instruments-hash)

(defun get-ir (name)
  (gethash (intern (car (om::list! (string name))) :phr-constraints) *instrument-ranges-hash*))
	  
(om::defmethod! orchestra-ranges ((instruments list))
 :initvals '( ((2 2 2 2) (4 3 3 1) (om::timpani 2) (om::harp) (2 1 1 1)) )
 :indoc '("list")
 :doc "Returns a list o SCREAMER-SCORE-DOMAINS objects, one for each instrument. 
The format is a list of lists consisting in:
- a list of numbers for woodwinds, for ex. (2 2 2 2) means 2 flutes, 2 oboes, 2 clarinets in Bb and 2 Bassons. 
- a list of numbers for brass, for ex. (4 3 3 1) means 4 horns, 3 trumpets in C, 3 trombones and tuba.
- a list of percussion instrument names or names and numbers (for unpitched instruments), for ex. (timpani 2) means timpani plus 2 pecussionists.
- a list of keyboard instruments or harp, for ex. (piano harp).
- a list with numbers for string instruments, for ex. (2 1 1 1) means 2 volins, viiola, cello and double bass."
 :icon 486 
 (om::flat 
(loop for family in instruments
         for x from 0
		 when family
         collect (cond ((= x 0) (loop for instrument in family
                                      for y from 0
									  when instrument
                                      collect (cond ((= 0 y)
										             (unless (zerop instrument)
										              (if (<= instrument 2)
                                                           (om::repeat-n (om::screamer-score-domain (get-ir 'flute) "notes") instrument)
														    (cons (om::screamer-score-domain (get-ir 'piccolo) "notes")
															  (om::repeat-n (om::screamer-score-domain (get-ir 'flute) "notes") (1- instrument))))))
                                                       ((= 1 y)
                                                         (unless (zerop instrument)
														  (if (<= instrument 2) 
															  (om::repeat-n (om::screamer-score-domain (get-ir 'oboe) "notes") instrument)
															  (om::x-append (om::repeat-n (om::screamer-score-domain (get-ir 'oboe) "notes") (1- instrument))
																        (om::screamer-score-domain (get-ir 'english-horn) "notes")))))
                                                       ((= 2 y)
                                                        (unless (zerop instrument)
														  (if (<= instrument 2) 
                                                              (om::repeat-n (om::screamer-score-domain (get-ir 'clarinet-in-bb) "notes") instrument)
															  (om::x-append (om::repeat-n (om::screamer-score-domain (get-ir 'clarinet-in-bb) "notes") (1- instrument))
																        (om::screamer-score-domain (get-ir 'bass-clarinet) "notes")))))
                                                       ((= 3 y)
                                                        (unless (zerop instrument)
														 (if (<= instrument 2) 
                                                              (om::repeat-n (om::screamer-score-domain (get-ir 'bassoon) "notes") instrument)
															  (om::x-append (om::repeat-n (om::screamer-score-domain (get-ir 'bassoon) "notes") (1- instrument))
																        (om::screamer-score-domain (get-ir 'contrabassoon) "notes"))))))))
								 ((= x 1)										
                                (loop for instrument in family
                                         for y from 0
                                         collect (cond ((= 0 y)(unless (zerop instrument) (om::repeat-n (om::screamer-score-domain (get-ir 'french-horn) "notes") instrument))
                                                                    )
                                                       ((= 1 y)(unless (zerop instrument)(om::repeat-n (om::screamer-score-domain (get-ir 'trumpet-in-c) "notes") instrument))
                                                                    )
                                                       ((= 2 y) (unless (zerop instrument)(om::repeat-n (om::screamer-score-domain (get-ir 'trombone) "notes") instrument)))
                                                       ((= 3 y) (unless (zerop instrument)(om::repeat-n (om::screamer-score-domain (get-ir 'tuba) "notes") instrument))))))
                       ((= x 2)  
                                (loop for instrument in family
                                      when instrument collect (if (symbolp instrument) 
                                                                  (om::screamer-score-domain (get-ir instrument) "notes")
                                                                  (om::repeat-n (om::screamer-score-domain '(60) "notes") instrument))))

                       ((= x 3)  (loop for instrument in family
                                             when instrument  collect (om::screamer-score-domain (get-ir instrument) "notes")))

                           ((= x 4)    (loop for instrument in family
                                                   for y from 0
                                                   collect (cond ((= 0 y) (unless (zerop instrument)(om::repeat-n (om::screamer-score-domain (get-ir 'violin) "notes") instrument)))
                                                       ((= 1 y)(om::repeat-n (om::screamer-score-domain (get-ir 'viola) "notes") instrument)
                                                                    )
                                                       ((= 2 y)(om::repeat-n (om::screamer-score-domain (get-ir 'cello) "notes") instrument)
                                                                    )
                                                       ((= 3 y) (om::repeat-n (om::screamer-score-domain (get-ir 'double-bass) "notes") instrument)
                                                                    ))))))))

(defvar *instrument-ranges-menu* nil)
(setf *instrument-ranges-menu* 
	    (loop for instrument-name in *all-instrument-ranges*
              collect (let ((string-name (string instrument-name)))
                       (om::repeat-n string-name 2))))				   

(om::defmethod! i-range ((instrument string) (notes/chords string) &optional n) 
 :initvals '("piano" "notes" (2 3 4 3))
 :indoc '("string" "string" "number or list")
 :menuins  (list (list 0 *instrument-ranges-menu*) (list 1 (list (list "notes" "notes") (list "chords" "chords"))))
 :doc "Returns a list o SCREAMER-SCORE-DOMAINS objects, one for each instrument."
 :icon 486 
  (om::screamer-score-domain (get-ir (read-from-string instrument)) notes/chords n))

;; TODO: CHANGE-INSTRUMENT-RANGE

  
