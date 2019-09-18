(defpackage proto-cl-harmony/t/harmony
  (:use :cl
        :proto-cl-harmony/js/harmony
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils)
  (:import-from :proto-cl-harmony/js/scale
                :make-scale)
  (:import-from :proto-cl-harmony/t/test-utils
                :same-list-p))
(in-package :proto-cl-harmony/t/harmony)

(deftest.ps+ for-make-harmony
  (let ((scale (make-scale :c :major))
        (list '((:normal 1 (:c :e :g))
                (:normal 5 (:g :b :d)))))
    (dolist (pair list)
      (let ((kind         (nth 0 pair))
            (num-in-scale (nth 1 pair))
            (expected     (nth 2 pair)))
        (ok (same-list-p (make-harmony scale num-in-scale kind)
                         expected))))))
