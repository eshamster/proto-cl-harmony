(defpackage proto-cl-harmony/t/scale
  (:use :cl
        :proto-cl-harmony/js/scale
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils))
(in-package :proto-cl-harmony/t/scale)

(defun.ps+ same-list-p (a b)
  (unless (= (length a) (length b))
    (return-from same-list-p nil))
  (loop
     :for x :in a
     :for y :in b
     :do (unless (eq x y)
           (return-from same-list-p nil)))
  t)

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
