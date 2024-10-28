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

;; INSTRUMENT RANGES
;; MIDI (LOW AND HIGH)

(defvar piccolo '(74 102))

(defvar flute '(60 96))

(defvar flute-b-foot '(59 96))

(defvar alto-flute '(55 91))

(defvar oboe '(58 91))

(defvar english-horn '(52 81))

(defvar clarinet-in-eb '(53 97))

(defvar clarinet-in-bb '(50 94))

;(defvar clarinet-in-a)

(defvar bass-clarinet '(38 77))

(defvar bassoon '(34 75))

(defvar contrabassoon '(22 53))

;(defvar sopranino-sax)

(defvar soprano-sax '(56 88))

(defvar alto-sax '(49 81))

(defvar tenor-sax '(44 76))

(defvar baritone-sax '(36 69))

;(defvar bass-sax)

;(defvar contrabass-sax)

(defvar french-horn '(34 77))

;(defvar trumpet-in-bb)

(defvar trumpet-in-c '(55 84))

(defvar trombone '(40 72))

(defvar bass-trombone '(34 67))

(defvar tuba '(28 58))

(defvar euphonium '(31 68))

(defvar timpani '(40 55))

(defvar xylophone '(65 108))

(defvar marimba '(45 96))

(defvar glockenspiel '(79 108))

(defvar vibraphone '(53 89))

;(defvar chimes)

(defvar harp '(24 103))

(defvar guitar '(40 88))

(defvar piano '(21 108))

(defvar celesta '(60 108))

(defvar harpsichord '(29 89))

(defvar violin '(55 103))

(defvar viola '(48 91))

(defvar cello '(36 76))

(defvar double-bass '(28 67))

(defvar bass-c-ext '(24 67))

(defvar *all-instrument-ranges* '(piccolo flute flute-b-foot alto-flute oboe english-horn clarinet-in-eb clarinet-in-bb bass-clarinet bassoon contrabassoon soprano-sax alto-sax tenor-sax baritone-sax french-horn trumpet-in-c trombone bass-trombone tuba euphonium timpani xylophone marimba glockenspiel vibraphone harp guitar piano celesta harpsichord violin viola cello double-bass bass-c-ext))

;===============================================================
(import  *all-instrument-ranges* :phr-constraints)

