(defpackage proto-cl-harmony/js/utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export
   ;; html
   :get-elem
   :get-value
   :set-inner
   :add-event-listener
   ;; string
   :split-string-to-list
   :concatenate-string))
(in-package :proto-cl-harmony/js/utils)

(enable-ps-experiment-syntax)

;; --- html --- ;;

(defun.ps get-elem (id)
  (#j.document.getElementById# id))

(defun.ps get-value (id)
  (@ (get-elem id) value))

(defun.ps set-inner (id value)
  (setf (@ (get-elem id) #j.innerHTML#)
        value))

(defun.ps add-event-listener (id kind callback)
  ((@ (get-elem id) add-event-listener) kind callback))

;; --- string --- ;;

(defun split-string-to-list (str)
  ;; "ABC" -> ("A" "B" "C")
  (mapcar #'string (coerce str 'list)))

(defun.ps-only split-string-to-list (str)
  (str.split ""))

(defun concatenate-string (&rest strings)
  (apply #'concatenate `(string ,@strings)))

(defun.ps-only concatenate-string (&rest strings)
  (let ((result ""))
    (dolist (str strings)
      (incf result str))
    result))
