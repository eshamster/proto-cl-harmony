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
        (list '((:make-kind :normal
                 :base-num  1
                 :base-tone :c
                 :tone-list (:c :e :g)
                 :out-kind  :tonic
                 :substitute-p nil)
                (:make-kind :normal
                 :base-num  2
                 :base-tone :d
                 :tone-list (:d :f :a)
                 :out-kind  :sub-dominant
                 :substitute-p t)
                (:make-kind :normal
                 :base-num  3
                 :base-tone :e
                 :tone-list (:e :g :b)
                 :out-kind  :tonic
                 :substitute-p t)
                (:make-kind :normal
                 :base-num  4
                 :base-tone :f
                 :tone-list (:f :a :c)
                 :out-kind  :sub-dominant
                 :substitute-p nil)
                (:make-kind :normal
                 :base-num  5
                 :base-tone :g
                 :tone-list (:g :b :d)
                 :out-kind  :dominant
                 :substitute-p nil)
                (:make-kind :normal
                 :base-num  6
                 :base-tone :a
                 :tone-list (:a :c :e)
                 :out-kind  :tonic
                 :substitute-p t)
                (:make-kind :normal
                 :base-num  7
                 :base-tone :b
                 :tone-list (:b :d :f)
                 :out-kind  :dominant
                 :substitute-p t))))
    (dolist (pair list)
      (let ((make-kind    (getf pair :make-kind))
            (num-in-scale (getf pair :base-num))
            ;; expected
            (base-tone    (getf pair :base-tone))
            (tone-list    (getf pair :tone-list))
            (out-kind     (getf pair :out-kind))
            (substitute-p (getf pair :substitute-p)))
        (testing (format nil "num-in-scale: ~D" num-in-scale)
          (let ((harmony (make-harmony-by scale num-in-scale make-kind)))
            (ok (eq (harmony-base-tone harmony) base-tone))
            (ok (same-list-p (harmony-tone-list harmony) tone-list))
            (ok (eq (harmony-kind harmony) out-kind))
            (if substitute-p
                (ok (harmony-substitute-p harmony))
                (ok (not (harmony-substitute-p harmony))))))))))
