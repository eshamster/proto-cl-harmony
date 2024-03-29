(defpackage proto-cl-harmony/server
  (:use :cl
        :cl-markup
        :proto-cl-harmony/js/main)
  (:export :start
           :stop))
(in-package :proto-cl-harmony/server)

;; --- Definitions about directories --- ;;

(defvar *script-dir*
  (merge-pathnames "static/"
                   (asdf:component-pathname
                    (asdf:find-system :proto-cl-harmony))))

(defvar *js-dir*
  (merge-pathnames "js/" *script-dir*))

(defvar *js-main-file*
  (merge-pathnames "main.js" *js-dir*))

;; --- Make js main file --- ;;

(defun make-js-main-file ()
  (with-open-file (out *js-main-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ
     (pse:with-use-ps-pack (:proto-cl-harmony/js/main))
     out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (make-js-main-file)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "proto-cl-harmony"))
                   (:body
                    (:h2 "Scale")
                    (:table
                     (:tr (:th "Base tone")
                          (:td (:select :id "scale-base-tone"
                                (dolist (tone '(:a :a+ :b :c :c+ :d :d+ :e :f :f+ :g :g+))
                                  (let ((tone-value (string-downcase (symbol-name tone))))
                                    (if (eq tone :c)
                                        (markup (:option :value tone-value :selected t tone))
                                        (markup (:option :value tone-value tone))))))))
                     (:tr (:th "Scale kind")
                          (:td (:select :id "scale-kind"
                                (dolist (kind '(:major :minor-natural
                                                :minor-harmonic :minor-melodic))
                                  (let ((kind-value (string-downcase (symbol-name kind))))
                                    (if (eq kind :major)
                                        (markup (:option :value kind-value :selected t kind))
                                        (markup (:option :value kind-value kind)))))))))
                    (:div
                     (:button :id "play-scale-btn" "Play Scale"))
                    (:div
                     (:input :id "scale-display" :type "text" :disabled t nil))

                    (:h2 "Melody by tiny MML (Music Macro Language)")
                    (:table
                     (:tr (:th "With Harmony")
                          (:td (:input :id "with-harmony" :type "checkbox")))
                     (:tr (:th "MML")
                          (:td (:textarea :id "mml-input" "<CDEFGAB<C")))
                     (:tr (:th "BPM")
                          (:td (:div :id "melody-bpm-value" 120)
                               (:input :id "melody-bpm" :type :range
                                       :min 20 :max 480 :value 120)))
                     (:tr (:th "Beat")
                          (:td (:select :id "beat-count"
                                (dolist (count '(1 2 3 4 5 6 7 8))
                                  (if (eq count 4)
                                      (markup (:option :value count :selected t count))
                                      (markup (:option :value count count)))))
                               " / "
                               (:select :id "beat-base"
                                (dolist (beat-base '(2 4 8))
                                  (if (eq beat-base 4)
                                      (markup (:option :value beat-base :selected t beat-base))
                                      (markup (:option :value beat-base beat-base))))))))
                    (:div
                     (:button :id "play-melody-btn" "Play Melody"))
                    (:table :id "measure-table" nil)

                    (:hr nil)

                    (:h2 "Harmony")
                    (:table
                     (:tr (:td (:button :id "play-harmony-1-btn" "I"))
                          (:td (:button :id "play-harmony-2-btn" "II"))
                          (:td (:button :id "play-harmony-3-btn" "III"))
                          (:td (:button :id "play-harmony-4-btn" "IV")))
                     (:tr (:td (:button :id "play-harmony-5-btn" "V"))
                          (:td (:button :id "play-harmony-6-btn" "VI"))
                          (:td (:button :id "play-harmony-7-btn" "VII"))))
                    (:script :src "js/main.js" nil)))))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun start (&key (port 5000) (address "0.0.0.0"))
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *script-dir*)
          *app*)
         :port port
         :address address)))
