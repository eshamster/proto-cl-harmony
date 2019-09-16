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

(deftest.ps+ for-tone<->number
  (dolist (pair '((:a  0   0 t)
                  (:a  0   0 nil)
                  (:a  1  12 t)
                  (:a -1 -12 t)
                  (:g+ 0  11 t)
                  (:a- 0  11 nil)
                  (:d+ 0   6 t)
                  (:d+ 1  18 t)))
    (let ((tone   (nth 0 pair))
          (octave (nth 1 pair))
          (number (nth 2 pair))
          (prefer-sharp-p (nth 3 pair)))
      (ok (within (tone-to-number tone octave) number))
      (multiple-value-bind (got-tone got-octave)
          (number-to-tone number prefer-sharp-p)
        (ok (eq got-tone   tone))
        (ok (=  got-octave octave))))))

(deftest.ps+ for-calc-tone-by-diff
  (dolist (pair '((:a 0   1   t :a+ 0)
                  (:a 0   1 nil :b- 0)
                  (:a 0  14   t :b  1)
                  (:a 0 -14   t :g -2)))
    (let ((tone   (nth 0 pair))
          (octave (nth 1 pair))
          (diff   (nth 2 pair))
          (prefer-sharp-p  (nth 3 pair))
          (expected-tone   (nth 4 pair))
          (expected-octave (nth 5 pair)))
      (multiple-value-bind (got-tone got-octave)
          (calc-tone-by-diff tone octave diff prefer-sharp-p)
        (ok (eq got-tone   expected-tone))
        (ok (=  got-octave expected-octave))))))
