;;;; run.lisp -- standalone entry point for the Common Lisp 3 regression suite.
;;;;
;;;; Requires no ASDF configuration; run it directly:
;;;;
;;;;     sbcl  --script tests/run.lisp
;;;;     ccl   -n -Q -l tests/run.lisp
;;;;     ecl   -q -norc -load tests/run.lisp
;;;;     abcl  --noinform --batch --load tests/run.lisp
;;;;     clisp -q -norc tests/run.lisp
;;;;
;;;; or, for every implementation installed on this machine, "make test".
;;;;
;;;; Exits non-zero only on UNEXPECTED results, so a suite carrying known
;;;; defects is still a regression signal for everything else.

(in-package "COMMON-LISP-USER")

(defparameter *test-dir*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *root-dir*
  (make-pathname :directory (butlast (pathname-directory *test-dir*))
                 :defaults *test-dir*))

;  cl3.lisp loads clos-utils and lisp1, and sets the readtable case.
(load (merge-pathnames "cl3.lisp"          *root-dir*))
(load (merge-pathnames "harness.lisp"      *test-dir*))
(load (merge-pathnames "regressions.lisp"  *test-dir*))

(defun exit-with (code)
  "Terminate with CODE.  This is also the reference implementation for the
portable exit helper the library provides as CL3:QUIT-LISP, which repq once
handled for SBCL and CLISP alone."
  (finish-output *standard-output*)
  (finish-output *error-output*)
  #+sbcl  (sb-ext:exit :code code)
  #+ccl   (ccl:quit code)
  #+ecl   (ext:quit code)
  #+clisp (ext:quit code)
  #+abcl  (ext:quit :status code)
  #-(or sbcl ccl ecl clisp abcl)
  (progn (format *error-output* "~&No portable exit for ~a; code was ~d.~%"
                 (lisp-implementation-type) code)
         code))

(exit-with (if (zerop (funcall (intern "RUN-TESTS" "CL3-TEST"))) 0 1))
