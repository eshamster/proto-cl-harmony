(defpackage proto-cl-harmony/js/sequencer
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :make-note
           :init-sequencer
           :start-sequencer
           :register-note-list
           :get-quater-note-tick))
(in-package :proto-cl-harmony/js/sequencer)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defvar.ps+ *quater-note-tick* 480)
(defvar.ps+ *monitor-ms* 100)
(defvar.ps+ *detect-ms* 200)

(defstruct.ps+ note
    freq
  start-tick
  resume-tick)

(defstruct.ps+ sequencer
    audioctx
  (t0 0) ; time when starting sequencer
  (note-queue (list)))

;; --- interface --- ;;

(defun.ps+ get-quater-note-tick ()
  *quater-note-tick*)

(defun.ps+ init-sequencer (audioctx)
  (make-sequencer :audioctx audioctx))

(defun.ps+ register-note-list (sequencer note-list)
  (queue-notes (sequencer-note-queue sequencer) note-list))

;; TODO: Enable restart and stop. Avoid duplicate running.
;; TODO: Enable to change BPM in playing

(defun.ps start-sequencer (sequencer bpm)
  (with-slots (t0 audioctx) sequencer
    (setf (sequencer-t0 sequencer) audioctx.current-time)
    (set-interval
     (lambda ()
       (let* ((cur-sec (- audioctx.current-time t0))
              (notes (dequeue-notes (sequencer-note-queue sequencer)
                                    (sec-to-tick cur-sec bpm)
                                    (sec-to-tick (/ *detect-ms* 1000) bpm))))
         (dolist (note notes)
           (let ((start-tick (note-start-tick note))
                 (resume-tick (note-resume-tick note))
                 (osc (new (#j.OscillatorNode# audioctx))))
             (setf osc.frequency.value (note-freq note))
             (chain osc
                    (connect audioctx.destination))
             (osc.start (+ t0 (tick-to-sec start-tick bpm)))
             (osc.stop  (+ t0 (tick-to-sec (+ start-tick resume-tick) bpm)))))))
     *monitor-ms*)))

;; --- internal --- ;;

;; - tick - ;;

(defun.ps+ sec-to-tick (sec bpm)
  (* sec (get-tick-per-sec bpm)))

(defun.ps+ tick-to-sec (tick bpm)
  (/ tick (get-tick-per-sec bpm)))

(defun.ps+ get-tick-per-sec (bpm)
  (let ((tick-per-min (* bpm *quater-note-tick*)))
    (/ tick-per-min 60)))

;; - queue - ;;

(defun.ps+ queue-notes (queue note-list)
  "Sort notes by its start-tick and queue them."
  (let ((copied-list (list)))
    (dolist (n note-list)
      (check-type n note)
      (push n copied-list))
    (setf copied-list
          (sort copied-list
                (lambda (a b)
                  (> (note-start-tick a)
                     (note-start-tick b)))))
    (dolist (n copied-list)
      (push n queue))))

(defun.ps+ dequeue-notes (queue current-tick detect-tick)
  "Dequeue notes if its start-tick is under current-tick+detect-tick"
  (labels ((rec (result)
             (let ((head (car queue)))
               (unless head
                 (return-from rec result))
               (if (< (note-start-tick head)
                      (+ current-tick detect-tick))
                   (rec (push (pop queue) result))
                   result))))
    (rec (list))))
