(defpackage proto-cl-harmony/js/const
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :get-tone-freq))
(in-package :proto-cl-harmony/js/const)

(enable-ps-experiment-syntax)

;; --- tone --- ;;

(defvar.ps+ *tone-table* (make-tone-table))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun.ps+ get-tone-freq (tone octave)
    (let ((base-freq (gethash tone *tone-table*)))
      (assert base-freq)
      (* base-freq (expt 2 octave))))

  (defun.ps+ make-tone-table ()
    (let ((table (make-hash-table))
          (freq 440)
          (scale-factor (expt 2 1/12)))
      (dolist (tones '((:a) (:a+ :b-)
                       (:b)
                       (:c) (:c+ :d-)
                       (:d) (:d+ :e-)
                       (:e)
                       (:f) (:f+ :g-)
                       (:g) (:g+ :a-)))
        (dolist (tone tones)
          (setf (gethash tone table) freq))
        (setf freq (* freq scale-factor)))
      table)))
