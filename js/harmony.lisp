(defpackage proto-cl-harmony/js/harmony
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :make-harmony-by
           :harmony
           :harmony-base-tone
           :harmony-tone-list
           :harmony-kind
           :harmony-substitute-p)
  (:import-from :proto-cl-harmony/js/sequencer
                :note-resume-tick
                :get-quater-note-tick))
(in-package :proto-cl-harmony/js/harmony)

(enable-ps-experiment-syntax)

(defstruct.ps+ harmony
    base-tone
  tone-list
  kind ; :tonic, :dominant, :sub-dominant
  substitute-p)

(defun.ps+ make-harmony-by (scale base-number &optional (kind :normal))
  "base-number is 1 to 7"
  (labels ((get-tone (number)
             (nth (mod (1- number) 7)
                  scale))
           (get-tone-list (base-number &rest diffs)
             (loop :for diff :in diffs
                :collect (get-tone (+ base-number diff)))))
    (ecase kind
      (:normal (make-harmony
                :base-tone (get-tone base-number)
                :tone-list (get-tone-list base-number 0 2 4)
                :kind (ecase base-number
                        ((1 3 6) :tonic)
                        ((5 7)   :dominant)
                        ((2 4)   :sub-dominant))
                :substitute-p (ecase base-number
                                ((1 4 5)   nil)
                                ((2 3 6 7) t)))))))
