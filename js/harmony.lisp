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
                :get-quater-note-tick)
  (:import-from :proto-cl-harmony/js/tone
                :tone-to-number
                :tone-to-string)
  (:import-from :proto-cl-harmony/js/utils
                :concatenate-string))
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

(defun.ps+ harmony-to-string (harmony)
  ;; Note: Only support basic 3 notes harmony
  (concatenate-string
   (tone-to-string (harmony-base-tone harmony))
   (tone-list-to-additional-chord-name (harmony-tone-list harmony))))

(defun.ps+ tone-list-to-additional-chord-name (tone-list)
  (let ((diff-list (tone-list-to-diff-list tone-list)))
    (case (length diff-list)
      (2 (let ((a (car  diff-list))
               (b (cadr diff-list)))
           (cond ((and (= a 4) (= b 3)) "")
                 ((and (= a 3) (= b 4)) "m")
                 ((and (= a 3) (= b 3)) "dim")
                 ((and (= a 4) (= b 4)) "aug")
                 (t (error "Not implemeted chord type: ~A" tone-list)))))
      (t (error "Not implemeted chord type: ~A" tone-list)))))

(defun.ps+ tone-list-to-diff-list (tone-list)
  ;; Ex1. (:c :e :g) -> (4 3)
  ;; Ex2. (:a :c :e) -> (3 4)
  (let ((number-list (list))
        (octave 0)
        (prev-number nil))
    (dolist (tone tone-list)
      (let ((number (tone-to-number tone octave)))
        (when (and prev-number
                   (< number prev-number))
          (incf octave)
          (setf number (tone-to-number tone octave)))
        (setf number-list
              (append number-list (list number)))
        (setf prev-number number)))
    (let ((result (list)))
      (dotimes (i (1- (length number-list)))
        (setf result
              (append result (list (- (nth (1+ i) number-list)
                                      (nth i      number-list))))))
      result)))
