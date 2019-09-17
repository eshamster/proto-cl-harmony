(defpackage proto-cl-harmony/js/main
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :proto-cl-harmony/js/tone
                :get-tone-freq
                :tone-to-number)
  (:import-from :proto-cl-harmony/js/scale
                :make-scale)
  (:import-from :proto-cl-harmony/js/sequencer
                :make-note
                :init-sequencer
                :start-sequencer
                :register-note-list
                :get-quater-note-tick)
  (:import-from :proto-cl-harmony/js/utils
                :get-value
                :add-event-listener))
(in-package :proto-cl-harmony/js/main)

(enable-ps-experiment-syntax)

(def-top-level-form.ps "initialize"
  (add-event-listener "play-scale-btn" "click" #'start-play-scale))

(defvar.ps+ *sequencer* nil)
(defvar.ps+ *playing-p* nil)

(defvar.ps+ *temp-interval* 0.5)

(defun.ps init-audioctx ()
  (new (#j.AudioContext#)))

(defun.ps+ start-play-scale ()
  (unless *sequencer*
    (setf *sequencer* (init-sequencer (init-audioctx))))
  (let* ((note-list (list))
         (tick (get-quater-note-tick))
         (base-tone (get-value "scale-base-tone"))
         (octave    (get-value "scale-octave"))
         (kind      (get-value "scale-kind"))
         (scale (make-scale base-tone kind))
         (i 0)
         (prev-tone-number (tone-to-number (car scale) octave)))
    (dolist (tone (append scale (car scale)))
      (when (< (tone-to-number tone octave) prev-tone-number)
        (incf octave))
      (push (make-note :freq (get-tone-freq tone octave)
                       :start-tick  (* i tick)
                       :resume-tick tick)
            note-list)
      (incf i)
      (setf prev-tone-number (tone-to-number tone octave)))
    (register-note-list *sequencer* note-list)
    (start-sequencer *sequencer* 120)))
