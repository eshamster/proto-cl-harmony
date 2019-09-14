(defpackage proto-cl-harmony/js/main
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :proto-cl-harmony/js/const
                :get-tone-freq)
  (:import-from :proto-cl-harmony/js/utils
                :add-event-listener))
(in-package :proto-cl-harmony/js/main)

(enable-ps-experiment-syntax)

(def-top-level-form.ps "initialize"
  (add-event-listener "play-btn" "click" #'start-play))

(defvar.ps+ *audioctx* nil)
(defvar.ps+ *playing-p* nil)

(defvar.ps+ *temp-interval* 0.5)

(defun.ps start-play ()
  (unless *audioctx*
    (setf *audioctx* (new (#j.AudioContext#))))
  (let ((index 0))
    (dolist (tone '((:c 0) (:d 0) (:e 0) (:f 0) (:g 0) (:a 1) (:b 1) (:c 1)))
      (let ((t0   *audioctx*.current-time)
            (osc  (new (#j.OscillatorNode# *audioctx*)))
            (gain (new (#j.GainNode# *audioctx*))))
        (setf osc.frequency.value (get-tone-freq (car tone) (cadr tone)))
        (chain osc
               (connect gain)
               (connect *audioctx*.destination))
        (osc.start (+ t0 (* *temp-interval* index)))
        (osc.stop  (+ t0 (* *temp-interval* (1+ index))))
        (incf index)))))
