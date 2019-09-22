(defpackage proto-cl-harmony/js/sequencer
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :note
           :make-note
           :init-sequencer
           :start-sequencer
           :register-note-list
           :get-quater-note-tick
           :calc-last-measure
           :calc-notes-in-measure)
  (:import-from :proto-cl-harmony/js/tone
                :get-tone-freq))
(in-package :proto-cl-harmony/js/sequencer)

(enable-ps-experiment-syntax)

;; --- data --- ;;

(defvar.ps+ *quater-note-tick* 480)
(defvar.ps+ *monitor-ms* 100)
(defvar.ps+ *detect-ms* 200)

(defstruct.ps+ note
    tone
  octave
  start-tick
  resume-tick)

(defstruct.ps+ sequencer
    audioctx
  (t0 0) ; time when starting sequencer
  (note-queue (list))
  (beat-count 4)
  (beat-base 4))

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
             (setf osc.frequency.value
                   (get-tone-freq (note-tone note)
                                  (note-octave note)))
             (chain osc
                    (connect audioctx.destination))
             (osc.start (+ t0 (tick-to-sec start-tick bpm)))
             (osc.stop  (+ t0 (tick-to-sec (+ start-tick resume-tick) bpm)))))))
     *monitor-ms*)))

;; - beat and measure - ;;

(defun.ps+ set-beat (sequencer count base)
  (setf (sequencer-beat-count sequencer) count
        (sequencer-beat-base  sequencer) base))

(defun.ps+ calc-last-measure (sequencer)
  (let ((last-tick 0))
    (dolist (note (peek-all-notes (sequencer-note-queue sequencer)))
      (let ((tail-tick (1- (+ (note-start-tick  note)
                              (note-resume-tick note)))))
        (when (> tail-tick last-tick)
          (setf last-tick tail-tick))))
    (1+ (floor (/ last-tick
                  (get-tick-per-measure (sequencer-beat-count sequencer)
                                        (sequencer-beat-base  sequencer)))))))

(defun.ps+ calc-notes-in-measure (sequencer measure)
  (when (> measure (calc-last-measure sequencer))
    (return-from calc-notes-in-measure (list)))
  (multiple-value-bind (measure-start-tick measure-end-tick)
      (get-tick-range-in-measure (sequencer-beat-count sequencer)
                                 (sequencer-beat-base  sequencer)
                                 measure)
    (let ((result-notes (list)))
      (dolist (note (peek-all-notes (sequencer-note-queue sequencer)))
        (let* ((start-tick (note-start-tick note))
               (end-tick   (+ start-tick (note-resume-tick note))))
          (when (overlap-range-p start-tick end-tick
                                 measure-start-tick measure-end-tick)
            (push note result-notes))))
      (reverse result-notes))))

;; --- internal --- ;;

;; - tick - ;;

(defun.ps+ sec-to-tick (sec bpm)
  (* sec (get-tick-per-sec bpm)))

(defun.ps+ tick-to-sec (tick bpm)
  (/ tick (get-tick-per-sec bpm)))

(defun.ps+ get-tick-per-sec (bpm)
  (let ((tick-per-min (* bpm *quater-note-tick*)))
    (/ tick-per-min 60)))

(defun.ps+ get-tick-per-measure (beat-count beat-base)
  (* beat-count
     (/ (* (get-quater-note-tick) 4)
        beat-base)))

(defun.ps+ get-tick-range-in-measure (beat-count beat-base measure)
  (let ((tick-per-measure (get-tick-per-measure beat-count beat-base)))
    (values (* tick-per-measure measure)
            (* tick-per-measure (1+ measure)))))

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

(defun.ps+ peek-all-notes (queue)
  queue)

;; - misc - ;;

(defun.ps+ overlap-range-p (min1 max1 min2 max2)
  "Judge if [min1, max1) and [min2, max2) overlap."
  (not (or (<= max1 min2)
           (<= max2 min1))))

