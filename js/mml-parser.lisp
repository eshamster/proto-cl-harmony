(defpackage proto-cl-harmony/js/mml-parser
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :tokenize-mml
           :parse-mml)
  (:import-from :proto-cl-harmony/js/sequencer
                :note
                :make-note
                :get-quater-note-tick)
  (:import-from :proto-cl-harmony/js/tone
                :calc-tone-by-diff)
  (:import-from :proto-cl-harmony/js/utils
                :split-string-to-list))
(in-package :proto-cl-harmony/js/mml-parser)

(enable-ps-experiment-syntax)

#|
Support tiny subset of MML (Music Macro Language)

<MML> := <block>...
<block> := <note>|<octave>

<note> := <tone>[<tone-changer>...][integer["."...]]
<tone> := "A"|"B"|"C"|"D"|"E"|"F"|"G"
<tone-changer> := "#"|"+"|"-"

<octave> := <set octave>|<inc octave>
<set octave> := "O"integer
<inc octave> := "<"|">"
|#

;; --- parser --- ;;

(defstruct.ps+ mml-state
    (tick (get-quater-note-tick))
  (sum-tick 0)
  (octave 4))

(defun.ps+ parse-mml (mml-str)
  "Parse tiny MML and return list of proto-cl-harmony/js/sequencer:note"
  (let ((token-list (tokenize-mml mml-str))
        (state (make-mml-state)))
    (parse-any state token-list (list))))

(defun.ps+ parse-any (state token-list note-list)
  (check-type state mml-state)
  (dolist (n note-list)
    (check-type n note))
  (if (car token-list)
      (ecase (getf (car token-list) :kind)
        (:note (parse-note state token-list note-list))
        (:octave (parse-octave state token-list note-list)))
      note-list))

(defun.ps+ parse-note (state token-list note-list)
  (let ((token (car token-list))
        (rest-token (cdr token-list)))
    (assert (eq (getf token :kind) :note))
    (let ((tone      (getf token :tone))
          (base-len  (getf token :len))
          (dot-count (getf token :dot-count)))
      (when base-len
        (setf (mml-state-tick state)
              (tone-len-to-tick base-len dot-count)))
      (let* ((octave (mml-octave-to-sequencer-octave
                      tone (mml-state-octave state)))
             (note (make-note
                    :tone tone :octave octave
                    :start-tick (mml-state-sum-tick state)
                    :resume-tick (mml-state-tick state))))
        (incf (mml-state-sum-tick state)
              (mml-state-tick state))
        (parse-any state rest-token
                   (append note-list (list note)))))))

(defun.ps+ parse-octave (state token-list note-list)
  (let ((token (car token-list))
        (rest-token (cdr token-list)))
    (assert (eq (getf token :kind) :octave))
    (let ((mode (getf token :mode))
          (value (getf token :value)))
      (ecase mode
        (:inc (incf (mml-state-octave state)
                    value))
        (:set (setf (mml-state-octave state)
                    value))))
    (parse-any state rest-token note-list)))

;; --- tokenizer --- ;;

(defun.ps+ tokenize-mml (mml-str)
  (tokenize-any (split-string-to-op mml-str) (list)))

(defun.ps+ tokenize-any (mml-ops  token-list)
  (let ((op (car mml-ops)))
    (if op
        (cond ((tone-op-p op)
               (tokenize-tone mml-ops token-list))
              ((octave-op-p op)
               (tokenize-octave mml-ops token-list))
              (t (error "Invalid MML. rest: ~A" mml-ops)))
        token-list)))

(defun.ps+ tokenize-tone (mml-ops token-list)
  (let ((base-tone (tone-str-to-tone (car mml-ops)))
        (rest-mml  (cdr mml-ops)))
    (labels ((extract-tone-changer (result)
               (let ((op (car rest-mml)))
                 (if (tone-change-op-p op)
                     (progn (pop rest-mml)
                            (extract-tone-changer
                             (+ result (if (string= op "-") -1 1))))
                     result)))
             (extract-tone-len ()
               (extractf-number rest-mml))
             (extract-dot-count (count)
               (let ((op (car rest-mml)))
                 (if (string= op ".")
                     (progn (pop rest-mml)
                            (extract-dot-count (1+ count)))
                     count))))
      (let ((tone      (calc-tone-by-diff
                        base-tone 0 (extract-tone-changer 0)))
            (len       (extract-tone-len))
            (dot-count (extract-dot-count 0)))
        (when (and (not len) (> dot-count 0))
          (error "Note length number is required before dot"))
        (tokenize-any
         rest-mml
         (append token-list
                 (list (list :kind :note :tone tone :len len
                             :dot-count dot-count))))))))

(defun.ps+ tokenize-octave (mml-ops token-list)
  (let* ((op (car mml-ops))
         (rest-mml (cdr mml-ops))
         (token
          (string-ecase op
            ("<" (list :kind :octave :mode :inc :value 1))
            (">" (list :kind :octave :mode :inc :value -1))
            ("O" (let ((num (extractf-number rest-mml)))
                   (unless num
                     (error "Number is required at next of \"O\""))
                   (list :kind :octave :mode :set :value num))))))
    (tokenize-any rest-mml
                  (append token-list (list token)))))

;; - extract - ;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun.ps+ extract-number (mml-ops)
    (let ((rest-mml mml-ops))
      (labels ((rec (num-op-list)
                 (let ((op (car rest-mml)))
                   (if (number-op-p op)
                       (progn (pop rest-mml)
                              (rec (append num-op-list (list op))))
                       (when (> (length num-op-list) 0)
                         (number-op-list-to-number num-op-list))))))
        (values (rec (list)) rest-mml)))))

(defmacro.ps+ extractf-number (mml-ops)
  `(multiple-value-bind (num rest)
       (extract-number ,mml-ops)
     (setf ,mml-ops rest)
     num))

;; --- operator --- ;;

(defun.ps+ tone-op-p (op)
  (find-string op (split-string-to-op "ABCDEFG")))

(defun.ps+ tone-change-op-p (op)
  (find-string op (split-string-to-op "+-#")))

(defun.ps+ number-op-p (op)
  (find-string op (split-string-to-op "0123456789")))

(defun.ps+ octave-op-p (op)
  (find-string op (split-string-to-op "<>O")))

;; --- utils --- ;;

;; - others - ;;

(defun.ps-only tone-str-to-tone (tone-str)
  (ecase tone-str
    ("A" :a) ("B" :b) ("C" :c) ("D" :d)
    ("E" :e) ("F" :f) ("G" :g)))

(defun tone-str-to-tone (tone-str)
  (intern tone-str (find-package "KEYWORD")))

(defmacro.ps+ string-ecase (var &body rest)
  (let ((g-var (gensym "VAR")))
    `(let ((,g-var ,var))
       (cond ,@(append (mapcar (lambda (line)
                                 `((string= ,g-var ,(car line))
                                   ,@(cdr line)))
                               rest)
                       `((t (error "Invalid string: ~A" ,g-var))))))))

(defun.ps+ tone-len-to-tick (base-len dot-count)
  (let* ((base-tick (/ (* (get-quater-note-tick) 4)
                       base-len))
         (tick base-tick))
    (dotimes (i dot-count)
      (incf tick (* base-tick (expt 1/2 (1+ i)))))
    tick))

(defun.ps+ mml-octave-to-sequencer-octave (tone mml-octave)
  ;; Some examples (mml octave -> sequencer octave)
  ;; :c  4 -> :c  -1
  ;; :g+ 4 -> :g+ -1
  ;; :a  4 -> :a  0
  ;; :b  4 -> :b  0
  ;; :c  5 -> :c  0
  (if (find tone '(:a :a+ :b))
      (- mml-octave 4)
      (- mml-octave 5)))

;; - string - ;;

(defun.ps+ find-string (str str-list)
  (find-if (lambda (x) (string= str x))
           str-list))

(defun split-string-to-op (str)
  ;; "AB C" -> ("A" "B" "C")
  (remove " " (mapcar #'string
                      (split-string-to-list (string-upcase str)))
          :test #'string=))

(defun.ps-only split-string-to-op (str)
  (let* ((upper-str (str.to-upper-case)))
    (split-string-to-list
     (upper-str.replace (regex "/ /g") ""))))

(defun number-op-list-to-number (op-list)
  (parse-integer (format nil "~{~A~}" op-list)))

(defun.ps-only number-op-list-to-number (op-list)
  (parse-int ((@ op-list reduce)
              (lambda (x y) (+ x y)))))
