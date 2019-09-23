(defpackage proto-cl-harmony/t/utils
  (:use :cl
        :proto-cl-harmony/js/utils
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils))
(in-package :proto-cl-harmony/t/utils)

(defun.ps+ same-string-list-p (a b)
  (unless (= (length a) (length b))
    (return-from same-string-list-p nil))
  (loop
     :for x :in a
     :for y :in b
     :do (unless (string= x y)
           (return-from same-string-list-p nil)))
  t)

(deftest.ps+ for-split-string-to-list
  (let ((list '(("Abc" ("A" "b" "c")))))
    (dolist (pair list)
      (let ((str      (nth 0 pair))
            (expected (nth 1 pair)))
        (ok (same-string-list-p
             (split-string-to-list str)
             expected))))))

(deftest.ps+ for-concatenate-string
  (let ((list '((("Ab" "cdE") "AbcdE"))))
    (dolist (pair list)
      (let ((str-list (nth 0 pair))
            (expected (nth 1 pair)))
        (ok (string= (apply #'concatenate-string str-list)
                     expected))))))
