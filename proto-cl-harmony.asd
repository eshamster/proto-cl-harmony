#|
  This file is a part of proto-cl-harmony project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  proto-cl-harmony is a prototype to add harmony to a melody

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem proto-cl-harmony
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :ningle
               :cl-markup
               :clack
               :proto-cl-harmony/main)
  :description "proto-cl-harmony is a prototype to add harmony to a melody"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op proto-cl-harmony/t))))

(defsystem proto-cl-harmony/t
  :class :package-inferred-system
  :depends-on (:ps-experiment-test
               :rove
               "ps-experiment/t/test-utils"
               "proto-cl-harmony/t/harmony"
               "proto-cl-harmony/t/scale"
               "proto-cl-harmony/t/tone")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
