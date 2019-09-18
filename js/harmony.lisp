(defpackage proto-cl-harmony/js/harmony
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :make-harmony))
(in-package :proto-cl-harmony/js/harmony)

(enable-ps-experiment-syntax)

(defun.ps+ make-harmony (scale base-number &optional (kind :normal))
  (ecase kind
    (:normal (list (nth (mod (1- (+ base-number 0)) 7) scale)
                   (nth (mod (1- (+ base-number 2)) 7) scale)
                   (nth (mod (1- (+ base-number 4)) 7) scale)))))
