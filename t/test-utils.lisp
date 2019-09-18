(defpackage proto-cl-harmony/t/test-utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :same-list-p))
(in-package :proto-cl-harmony/t/test-utils)

(defun.ps+ same-list-p (a b)
  (unless (= (length a) (length b))
    (return-from same-list-p nil))
  (loop
     :for x :in a
     :for y :in b
     :do (unless (eq x y)
           (return-from same-list-p nil)))
  t)
