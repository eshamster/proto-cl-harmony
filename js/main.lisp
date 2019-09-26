(defpackage proto-cl-harmony/js/main
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :proto-cl-harmony/js/tone
                :tone-to-number)
  (:import-from :proto-cl-harmony/js/scale
                :make-scale)
  (:import-from :proto-cl-harmony/js/harmony
                :make-harmony-by
                :harmony-tone-list
                :weighted-harmony-harmony
                :weighted-harmony-weight
                :calc-candidate-harmony-with-weight
                :select-harmonies
                :harmony-to-string)
  (:import-from :proto-cl-harmony/js/mml-parser
                :parse-mml)
  (:import-from :proto-cl-harmony/js/sequencer
                :make-note
                :note-tone
                :note-octave
                :note-resume-tick
                :init-sequencer
                :start-sequencer
                :register-note-list
                :get-quater-note-tick
                :get-measure-tick
                :calc-last-measure
                :calc-notes-in-measure)
  (:import-from :proto-cl-harmony/js/utils
                :get-elem
                :get-value
                :add-event-listener
                :set-inner))
(in-package :proto-cl-harmony/js/main)

(enable-ps-experiment-syntax)

(defvar.ps+ *sequencer* nil)
(defvar.ps+ *playing-p* nil)

(defvar.ps+ *harmony-velocity* 48)

(def-top-level-form.ps "initialize"
  (add-event-listener "play-scale-btn" "click" #'start-play-scale)
  (dotimes (i 7)
    (let ((num-in-scale (1+ i)))
      (add-event-listener (+ "play-harmony-" num-in-scale "-btn") "click"
                          (lambda ()
                            (start-play-harmony num-in-scale)))))
  (add-event-listener "play-melody-btn" "click"
                      (lambda ()
                        (start-play-meolody (get-value "mml-input")))))

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
      (push (make-note :tone tone
                       :octave octave
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
  (let ((tick (* 2 (get-quater-note-tick)))
        (bpm 120)
        (scale (make-scale-by-input)))
    (add-harmony-to-sequencer :sequencer   *sequencer*
                              :harmony     (make-harmony-by scale num-in-scale)
                              :start-tick  0
                              :resume-tick tick)
    (start-sequencer *sequencer* bpm)))

(defun.ps+ add-harmony-to-sequencer (&key sequencer harmony start-tick resume-tick)
  (let ((note-list (list)))
    (dolist (tone (harmony-tone-list harmony))
      (push (make-note :tone        tone
                       :octave      0
                       :start-tick  start-tick
                       :resume-tick resume-tick
                       :velocity    *harmony-velocity*)
            note-list))
    (register-note-list sequencer note-list)))

(defun.ps start-play-meolody (mml-str)
  (init-sequencer-if-requied)
  (let ((bpm 120))
    (try (progn
           (register-note-list *sequencer* (parse-mml mml-str))
           (let ((notes-in-measures (list))
                 (weighted-harmony-lists (list))
                 (selected-harmoies nil)
                 (scale (make-scale-by-input)))
             (dotimes (measure (calc-last-measure *sequencer*))
               (let ((notes (calc-notes-in-measure *sequencer* measure)))
                 (push notes notes-in-measures)
                 (push (calc-candidate-harmony-with-weight scale notes)
                       weighted-harmony-lists)))
             (setf notes-in-measures (reverse notes-in-measures))
             (setf weighted-harmony-lists (reverse weighted-harmony-lists))
             (setf selected-harmoies (select-harmonies weighted-harmony-lists))
             (let ((inner ""))
               (macrolet ((with-tag (tag &body body)
                            `(progn (incf inner ,(format nil "<~A>"  tag))
                                    ,@body
                                    (incf inner ,(format nil "</~A>" tag)))))
                 (with-tag "tr"
                   (with-tag "th"
                     (incf inner "Measure"))
                   (with-tag "th"
                     (incf inner "Notes"))
                   (with-tag "th"
                     (incf inner "Candidate Harmonies"))
                   (with-tag "th"
                     (incf inner "Selected")))
                 (dotimes (measure (length notes-in-measures))
                   (with-tag "tr"
                     (with-tag "td"
                       (incf inner (1+ measure)))
                     (let ((notes-in-measure (nth measure notes-in-measures)))
                       (with-tag "td"
                         (dolist (note notes-in-measure)
                           (incf inner (note-tone note))
                           (incf inner (tick-to-len (note-resume-tick note)))
                           (incf inner ","))))
                     (let ((candidates (nth measure weighted-harmony-lists)))
                       (with-tag "td"
                         (dolist (c candidates)
                           (incf inner (harmony-to-string c.harmony))
                           (incf inner (+ "(" (c.weight.to-fixed 1) "),")))))
                     (with-tag "td"
                       (incf inner (harmony-to-string (nth measure selected-harmoies)))))))
               (set-inner "measure-table" inner))
             ;; Add harmony and play
             (when (@ (get-elem "with-harmony") checked)
               (let ((harmony-tick (get-measure-tick *sequencer*)))
                 (dotimes (i (length selected-harmoies))
                   (add-harmony-to-sequencer
                    :sequencer   *sequencer*
                    :harmony     (nth i selected-harmoies)
                    :start-tick  (* i harmony-tick)
                    :resume-tick harmony-tick))))
             (start-sequencer *sequencer* bpm)))
         (:catch (e) (alert (+ "Error: " e))))))

;; TODO: Process dot. (Ex. 480 + 240 -> 4.)
(defun.ps+ tick-to-len (tick)
  (/ (* 4 (get-quater-note-tick))
     tick))
