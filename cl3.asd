;;;; cl3.asd -- ASDF system definition for Common Lisp 3
;;;;
;;;;     (asdf:load-system "cl3")
;;;;     (use-package :cl3)
;;;;
;;;; and to run the regression suite:
;;;;
;;;;     (asdf:test-system "cl3")
;;;;
;;;; The system remains loadable without ASDF; (load "cl3") loads the same
;;;; parts from source, in the same order.  readtable.lisp comes last in
;;;; both routes, so that the system's own source is read with the standard
;;;; readtable case and only user code afterwards is case-sensitive.

(defsystem "cl3"
  :description "ANSI Common Lisp with a single namespace, a layer over CLOS,
and case-sensitive symbols."
  :author "Blake McBride <blake@mcbridemail.com>"
  :licence "Public Domain"
  :version "0.2.0"
  :components ((:file "package")
               (:file "clos-utils" :depends-on ("package"))
               (:file "lisp1"      :depends-on ("package"))
               (:file "readtable"  :depends-on ("clos-utils" "lisp1")))
  :in-order-to ((test-op (test-op "cl3/tests"))))

(defsystem "cl3/tests"
  :description "Regression suite for Common Lisp 3: one test per numbered audit
finding, plus acceptance tests for the behaviour each fix introduced.  The
harness supports expected failures, though no test currently needs one."
  :author "Blake McBride <blake@mcbridemail.com>"
  :licence "Public Domain"
  :depends-on ("cl3")
  :components ((:module "tests"
                :components ((:file "harness")
                             (:file "regressions" :depends-on ("harness")))))
  :perform (test-op (op sys)
             (declare (ignore op sys))
             (let ((unexpected (uiop:symbol-call "CL3-TEST" "RUN-TESTS")))
               (unless (zerop unexpected)
                 (error "~d unexpected test result~:p." unexpected)))))
