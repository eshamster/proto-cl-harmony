(defpackage proto-cl-harmony/js/main
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :proto-cl-harmony/js/tone
                :get-tone-freq
                :tone-to-number)
  (:import-from :proto-cl-harmony/js/scale
                :make-scale)
  (:import-from :proto-cl-harmony/js/harmony
                :make-harmony)
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

(defvar.ps+ *sequencer* nil)
(defvar.ps+ *playing-p* nil)

(def-top-level-form.ps "initialize"
  (add-event-listener "play-scale-btn" "click" #'start-play-scale)
  (dotimes (i 7)
    (let ((num-in-scale (1+ i)))
      (add-event-listener (+ "play-harmony-" num-in-scale "-btn") "click"
                          (lambda ()
                            (start-play-harmony num-in-scale))))))

;; --- utils --- ;;

(defun.ps init-sequencer-if-requied ()
  (unless *sequencer*
    (setf *sequencer* (init-sequencer (new (#j.AudioContext#))))))

(defun.ps+ make-scale-by-input ()
  (let ((base-tone (get-value "scale-base-tone"))
        (kind      (get-value "scale-kind")))
    (make-scale base-tone kind)))

;; --- player --- ;;

(defun.ps+ start-play-scale ()
  (init-sequencer-if-requied)
  (let* ((note-list (list))
         (bpm 120)
         (tick (get-quater-note-tick))
         (octave (get-value "scale-octave"))
         (scale (make-scale-by-input))
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
    (start-sequencer *sequencer* bpm)))

(defun.ps+ start-play-harmony (num-in-scale)
  (unless *sequencer*
    (init-sequencer-if-requied))
  (let ((note-list (list))
        (tick (get-quater-note-tick))
        (bpm 120)
        (scale (make-scale-by-input)))
    (dolist (tone (make-harmony scale num-in-scale))
      (push (make-note :freq (get-tone-freq tone 0)
                       :start-tick  0
                       :resume-tick (* tick 2))
            note-list))
    (register-note-list *sequencer* note-list)
    (start-sequencer *sequencer* bpm)))
