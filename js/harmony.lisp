(defpackage proto-cl-harmony/js/harmony
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :make-harmony-by
           :harmony
           :harmony-base-tone
           :harmony-tone-list
           :harmony-kind
           :harmony-substitute-p
           :weighted-harmony
           :weighted-harmony-harmony
           :weighted-harmony-weight
           :calc-candidate-harmony-with-weight
           :harmony-to-string
           :select-harmonies)
  (:import-from :proto-cl-harmony/js/sequencer
                :note-tone
                :note-resume-tick
                :get-quater-note-tick)
  (:import-from :proto-cl-harmony/js/tone
                :tone-to-number
                :tone-to-string)
  (:import-from :proto-cl-harmony/js/utils
                :concatenate-string))
(in-package :proto-cl-harmony/js/harmony)

(enable-ps-experiment-syntax)

;; --- basic --- ;;

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

;; --- calc candidates --- ;;

(defstruct.ps+ weighted-harmony harmony (weight 0))

(defun.ps+ calc-candidate-harmony-with-weight (scale notes-in-measure)
  (let ((candidates (loop :for i :from 1 :to 7
                       :collect (make-weighted-harmony
                                 :harmony (make-harmony-by scale i)))))
    (when (= (length notes-in-measure) 0)
      (dolist (candidate candidates)
        (setf (weighted-harmony-weight candidate) 1))
      (return-from calc-candidate-harmony-with-weight candidates))
    (dolist (c candidates)
      (dotimes (i (length notes-in-measure))
        (let ((note (nth i notes-in-measure)))
          (incf (weighted-harmony-weight c)
                (calc-score-of-harmony-by-tone
                 :harmony          (weighted-harmony-harmony c)
                 :tone             (note-tone note)
                 :note-resume-tick (note-resume-tick note)
                 :first-tone-p     (= i 0))))))
    (sort (remove-if (lambda (c) (= (weighted-harmony-weight c) 0))
                     candidates)
          (lambda (a b)
            (> (weighted-harmony-weight a)
               (weighted-harmony-weight b))))))

;; Not well-tuned values
(defparameter.ps+ *score-table*
    (let ((table (make-hash-table)))
      (dolist (pair '((:included-tone 1)
                      ;; Note: tone-len is represented as
                      ;;       a relative value to quater note.
                      (:scale-for-tone-len 0.8)
                      (:base-tone 2)
                      (:first-tone 4) ; first in a measure
                      (:not-substitute 1)))
        (setf (gethash (car pair) table)
              (cadr pair)))
      table))

(defun.ps+ calc-score-of-harmony-by-tone (&key harmony
                                               tone
                                               note-resume-tick
                                               first-tone-p
                                               (score-table *score-table*))
  (unless (harmony-include-tone-p harmony tone)
    (return-from calc-score-of-harmony-by-tone 0))
  (flet ((get-score (key)
           (gethash key score-table)))
    (let ((score 0))
      (incf score
            (* (get-score :included-tone)
               (get-score :scale-for-tone-len)
               (/ note-resume-tick (get-quater-note-tick))))
      (when (eq (harmony-base-tone harmony) tone)
        (incf score (get-score :base-tone)))
      (when first-tone-p
        (incf score (get-score :first-tone)))
      (unless (harmony-substitute-p harmony)
        (incf score (get-score :not-substitute)))
      score)))

(defun.ps+ harmony-include-tone-p (harmony tone)
  (find tone (harmony-tone-list harmony)))

;; --- select harmony --- ;;

;; TODO: Reconsider interface

(defun.ps+ select-harmonies (weighted-harmony-lists &optional (playout 3))
  (labels ((rec ()
             ;; XXX: Prevent too many recursions
             (let ((result (list))
                   (prev-harmony nil)
                   (sum-weight 0))
               (dolist (whl weighted-harmony-lists)
                 (let ((wh (select-harmony whl prev-harmony)))
                   (if wh
                       (let ((w (weighted-harmony-weight  wh))
                             (h (weighted-harmony-harmony wh)))
                         (incf sum-weight w)
                         (push h result)
                         (setf prev-harmony h))
                       (return-from rec (rec)))))
               (values (reverse result) sum-weight))))
    (let ((max-weight 0)
          results)
      (dotimes (i playout)
        (multiple-value-bind (harmony-list weight) (rec)
          (when (> weight max-weight)
            (setf results    harmony-list
                  max-weight weight))))
      results)))

(defun.ps+ select-harmony (weighted-harmony-list prev-harmony)
  (let ((sum 0))
    (dolist (wh weighted-harmony-list)
      (incf sum (weighted-harmony-weight wh)))
    (let ((selector (* sum (rand1)))
          (base 0))
      (dolist (wh weighted-harmony-list)
        (let ((next (+ base (weighted-harmony-weight wh))))
          (when (< selector next)
            (return-from select-harmony
              (let ((h (weighted-harmony-harmony wh)))
                (unless (prohibited-p prev-harmony h)
                  wh))))
          (setf base next))))))

(defun rand1 ()
  (random 1.0))

(defun.ps-only rand1 ()
  (random))

;; Some prohibited progressions are excluded
;;   1. D (Dominant) -> SD (Sub Dominant)
;;   2. Substitute chord -> Dominant chord
(defun.ps+ prohibited-p (prev-harmony next-harmony)
  (when prev-harmony
    (or (and (eq (harmony-kind prev-harmony) :dominant)
             (eq (harmony-kind next-harmony) :sub-dominant))
        (and (eq (harmony-kind prev-harmony)
                 (harmony-kind next-harmony))
             (harmony-substitute-p prev-harmony)
             (not (harmony-substitute-p next-harmony))))))

;; --- utils --- ;;

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
