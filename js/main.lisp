(defpackage proto-cl-harmony/js/main
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :proto-cl-harmony/js/tone
                :get-tone-freq)
  (:import-from :proto-cl-harmony/js/sequencer
                :make-note
                :init-sequencer
                :start-sequencer
                :register-note-list
                :get-quater-note-tick)
  (:import-from :proto-cl-harmony/js/utils
                :add-event-listener))
(in-package :proto-cl-harmony/js/main)

(enable-ps-experiment-syntax)

(def-top-level-form.ps "initialize"
  (add-event-listener "play-btn" "click" #'start-play))

(defvar.ps+ *sequencer* nil)
(defvar.ps+ *playing-p* nil)

(defvar.ps+ *temp-interval* 0.5)

(defun.ps init-audioctx ()
  (new (#j.AudioContext#)))

(defun.ps+ start-play ()
  (unless *sequencer*
    (setf *sequencer* (init-sequencer (init-audioctx))))
  (let ((note-list (list))
        (tick (get-quater-note-tick))
        (i 0))
    (dolist (tone '((:c 0) (:d 0) (:e 0) (:f 0) (:g 0) (:a 1) (:b 1) (:c 1)))
      (push (make-note :freq (get-tone-freq (car tone) (cadr tone))
                       :start-tick  (* i tick)
                       :resume-tick tick)
            note-list)
      (incf i))
    (register-note-list *sequencer* note-list)
    (start-sequencer *sequencer* 120)))
