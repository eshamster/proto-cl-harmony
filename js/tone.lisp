(defpackage proto-cl-harmony/js/tone
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :get-tone-freq
           :tone-to-number
           :number-to-tone
           :calc-tone-by-diff
           :sharp-tone-p
           :flat-tone-p
           :tone-to-string)
  (:import-from :proto-cl-harmony/js/utils
                :split-string-to-list
                :concatenate-string))
(in-package :proto-cl-harmony/js/tone)

(enable-ps-experiment-syntax)

(defvar.ps+ *tone-name-list*
    '((:a) (:a+ :b-)
      (:b)
      (:c) (:c+ :d-)
      (:d) (:d+ :e-)
      (:e)
      (:f) (:f+ :g-)
      (:g) (:g+ :a-)))

(defun.ps+ get-tone-freq (tone octave)
  (let ((base-freq (gethash tone *tone-table*)))
    (assert base-freq)
    (* base-freq (expt 2 octave))))

(defun.ps+ calc-tone-by-diff (tone octave diff &optional (prefer-sharp-p t))
  "Calculate tone and octave.
The \"diff\" is number of half-tone from base \"tone\" and \"octave\"."
  (number-to-tone (+ (tone-to-number tone octave)
                     diff)
                  prefer-sharp-p))

(defun.ps+ tone-to-number (tone octave)
  "Convert tone and octave to number.
The number 0 represents 440Hz A (= tone :a and octave 0).
Increment it whenver half-tone is added.
Decrement it whenver half-tone is subtracted."
  (dotimes (i (length *tone-name-list*))
    (when (find tone (nth i *tone-name-list*))
      (return-from tone-to-number (+ i (* 12 octave)))))
  (error "The tone name \"~A\" is invalid" tone))

(defun.ps+ number-to-tone (number &optional (prefer-sharp-p t))
  "Convert number to tone and octave.
See the document of tone-to-number for detail."
  (let* ((octave     (floor (/ number 12)))
         (tone-index (mod number 12))
         (tone-list  (nth tone-index *tone-name-list*)))
    (assert tone-list)
    (values (if (= (length tone-list) 2)
                (if prefer-sharp-p
                    (car tone-list)
                    (cadr tone-list))
                (car tone-list))
            octave)))

(defun.ps+ sharp-tone-p (tone)
  (let ((tone-list (find-tone-list tone)))
    (and (= (length tone-list) 2)
         (eq tone (car tone-list)))))

(defun.ps+ flat-tone-p (tone)
  (let ((tone-list (find-tone-list tone)))
    (and (= (length tone-list) 2)
         (eq tone (cadr tone-list)))))

(defun.ps+ find-tone-list (tone)
  (dolist (tone-list *tone-name-list*)
    (when (find tone tone-list)
      (return-from find-tone-list tone-list)))
  (error "The tone name \"~A\" is invalid" tone))

(defun.ps+ make-tone-table ()
  (let ((table (make-hash-table))
        (freq 440)
        (scale-factor (expt 2 1/12)))
    (dolist (tones *tone-name-list*)
      (dolist (tone tones)
        (setf (gethash tone table) freq))
      (setf freq (* freq scale-factor)))
    table))

(defvar.ps+ *tone-table* (make-tone-table))

(defun.ps+ tone-to-string (tone)
  ;; Note: Only support no or one sharp [flat]
  (let ((splitted (split-keyword-to-string-list tone)))
    (case (length splitted)
      (0 (error "The tone is empty"))
      (1 (car splitted))
      (2 (let ((sign (cadr splitted)))
           (cond ((or (string= sign "+")
                      (string= sign "#"))
                  (concatenate-string (car splitted) "♯"))
                 ((or (string= sign "-"))
                  (concatenate-string (car splitted) "♭"))
                 (t (error "Not recognized sign: ~A" signe))))))))

(defun split-keyword-to-string-list (keyword)
  ;; :c+ -> ("C" "+")
  (split-string-to-list (symbol-name keyword)))

(defun.ps-only split-keyword-to-string-list (keyword)
  (split-string-to-list (keyword.to-upper-case)))
