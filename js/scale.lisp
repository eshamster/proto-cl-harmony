(defpackage proto-cl-harmony/js/scale
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :make-scale)
  (:import-from :proto-cl-harmony/js/tone
                :flat-tone-p
                :calc-tone-by-diff))
(in-package :proto-cl-harmony/js/scale)

(enable-ps-experiment-syntax)

(defvar.ps+ *half-tone-interval-table*
    (let ((list '((:major          (2 2 1 2 2 2))
                  (:minor-natural  (2 1 2 2 1 2))
                  (:minor-harmonic (2 1 2 2 1 3))
                  (:minor-melodic  (2 1 2 2 2 2))))
          (table (make-hash-table)))
      (dolist (pair list)
        (setf (gethash (car pair) table)
              (cadr pair)))
      table))

(defun.ps+ make-scale (base-tone kind)
  (let ((prefer-sharp-p (not (flat-tone-p base-tone)))
        (next-tone base-tone)
        (scale (list))
        (half-tone-intervals (gethash kind *half-tone-interval-table*)))
    (unless half-tone-intervals
      (error "The scale kind \"~A\" is not supported." kind))
    (push next-tone scale)
    (dolist (intv half-tone-intervals)
      (setf next-tone (calc-tone-by-diff next-tone 0 intv prefer-sharp-p))
      (push next-tone scale))
    (reverse scale)))

