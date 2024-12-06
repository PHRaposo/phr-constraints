;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIBRARY
;;; 
;;; DESCRIPTION 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load
;--------------------------------------------------

(defvar *phr-constraints-files* nil)
(setf  *phr-constraints-files* (list
				 (om-relative-path '("sources") "package")
		                 (om-relative-path '("sources") "utils")	 
		                 (om-relative-path '("sources") "phr-constraints")
						 (om::om-relative-path '("sources") "analysis-tools")
		                 (om-relative-path '("sources") "all-instrument-ranges")
		                 (om-relative-path '("sources") "instrument-ranges")
		                 ;(om-relative-path '("sources") "print-screamer-score") ;<== IN-PROGRESS						 
		                 ;(om-relative-path '("sources") "guitar-playable-chord") ;<== IN-PROGRESS						 						 						 
                               ))

;--------------------------------------------------
;Loading files
;--------------------------------------------------
(mapc #'compile&load *phr-constraints-files*)

;--------------------------------------------------
;Fill library
;--------------------------------------------------

(fill-library '(("SCREAMER-SCORE" nil nil (print-screamer-score) nil)
                ("MELODIC" nil nil (allowed-melodic-intervals constraint-motifs constraint-scale mel-line-intervals) nil)
		        ("HARMONIC" nil nil (no-crossing no-octaves chords-alldiff symmetrical-chords? chord-at-measure chord-at-times not-parallel-fifths-octaves not-parallel-intervals) nil)
				("UTILS" nil nil (get-fn collect-constraints all-true? list-memberv? all-notv-memberv all-ascendingv all-descendingv list-maxv list-minv om-mod om-rem mk-poly simple->poly
						          phr-constraints::orchestra-ranges phr-constraints::i-range) nil)			
			    ))
#|
;MODEL 1 - NO SUBFOLDER -> (library (functions))

(fill-library '((Nil Nil Nil (package::function) Nil)))

;MODEL 3 - SUBFOLDER -> (library (folder (fuctions)))

(fill-library '(("FOLDER1" nil nil (package::function1 package::function2 etc) nil)
				("FOLDER2" nil nil (package::function1 package::function2 etc) nil)
				("ETC..." nil nil (package::function1 package::function2 etc) nil)				
			   ))
	
;MODEL 3 - SUBFOLDERS WITH SUBFOLDERS -> (library (folder (subfolder (fuctions)))) 
(fill-library '(("FOLDER1"
				  (("SUBFOLDER1" nil nil (package::fun1 package::fun2 etc.) nil)
				    ("SUBFOLDER2" nil nil (package::fun1 package::fun2 etc.) nil)
				    ("ETC..." nil nil (package::fun1 package::fun2 etc.) nil)					
				   ) Nil Nil Nil)
				("FOLDER2" nil nil (package::function1 package::function2 etc) nil) ;<== with no subfolders				   
                 ))
|#
				 
(print (format nil "
PHR-CONSTRAINTS 
by Paulo Henrique Raposo - 2024"
))
