(defpackage proto-cl-harmony/t/mml-parser
  (:use :cl
        :proto-cl-harmony/js/mml-parser
        :rove
        :ps-experiment
        :parenscript
        :ps-experiment/t/test-utils)
  (:import-from :proto-cl-harmony/t/test-utils
                :same-list-p))
(in-package :proto-cl-harmony/t/mml-parser)

(deftest.ps+ for-tokenize-mml
  (let ((list '(("A b+8C--16.."
                 ((:KIND :NOTE :TONE :A :LEN NIL :DOT-COUNT 0)
                  (:KIND :NOTE :TONE :C :LEN 8 :DOT-COUNT 0)
                  (:KIND :NOTE :TONE :A+ :LEN 16 :DOT-COUNT 2)))
                ("O2<>"
                 ((:KIND :OCTAVE :MODE :SET :VALUE 2)
                  (:KIND :OCTAVE :MODE :INC :VALUE 1)
                  (:KIND :OCTAVE :MODE :INC :VALUE -1))))))
    (dolist (pair list)
      (let ((mml-str  (nth 0 pair))
            (expected (nth 1 pair)))
        (let ((result (tokenize-mml mml-str)))
          (ok (= (length result) (length expected)))
          (when (= (length result) (length expected))
            (dotimes (i (length result))
              (ok (same-list-p (nth i result) (nth i expected))))))))))

;; TODO test parse-mml
