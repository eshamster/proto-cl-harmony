(defpackage proto-cl-harmony/t/scale
  (:use :cl
        :proto-cl-harmony/js/scale
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils)
  (:import-from :proto-cl-harmony/t/test-utils
                :same-list-p))
(in-package :proto-cl-harmony/t/scale)

(deftest.ps+ for-make-scale
  (let ((list '((:major :c  (:c  :d  :e  :f  :g  :a  :b))
                (:major :c+ (:c+ :d+ :f  :f+ :g+ :a+ :c))
                (:major :d- (:d- :e- :f  :g- :a- :b- :c))
                (:minor-natural  :a (:a  :b  :c  :d  :e  :f  :g))
                (:minor-harmonic :a (:a  :b  :c  :d  :e  :f  :g+))
                (:minor-melodic  :a (:a  :b  :c  :d  :e  :f+ :g+)))))
    (dolist (pair list)
      (let ((kind      (nth 0 pair))
            (base-tone (nth 1 pair))
            (expected  (nth 2 pair)))
        (ok (same-list-p (make-scale base-tone kind) expected))))))
