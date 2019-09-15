(defpackage proto-cl-harmony/t/tone
  (:use :cl
        :proto-cl-harmony/js/tone
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils))
(in-package :proto-cl-harmony/t/tone)

(defun.ps+ within (got expected &optional (tolerance 0.001))
  (< (abs (- got expected))
     tolerance))

(deftest.ps+ for-get-tone-freq
  (dolist (pair '((:a  0 440)
                  (:a  1 880)
                  (:a -1 220)
                  (:d+ 0 622.254)
                  (:d+ 1 1244.508)))
    (let ((tone     (nth 0 pair))
          (octave   (nth 1 pair))
          (expected (nth 2 pair)))
      (ok (within (get-tone-freq tone octave) expected)))))
