;;;; harness.lisp -- a minimal, dependency-free test harness for Common Lisp 3.
;;;;
;;;; Deliberately small: the system it tests is a handful of portable ANSI files
;;;; with no dependencies, and the test suite keeps that property.
;;;;
;;;; The harness supports EXPECTED FAILURES (xfail), so that a suite can be
;;;; written against the *corrected* behaviour before the corrections land.
;;;; A test marked :xfail that fails is a
;;;; known defect and does not break the build; a test marked :xfail that
;;;; passes is reported as XPASS, meaning a fix has landed and the marker
;;;; should be removed.  Only unexpected results set a non-zero exit code, so
;;;; CI is a genuine regression signal from day one.
;;;;
;;;; :xfail accepts T (fails everywhere) or a list of implementation keywords
;;;; such as (:ccl), for the implementation-specific findings.

(defpackage "CL3-TEST"
  (:use "COMMON-LISP")
  (:export "DEFTEST" "IS-EQUAL" "IS-TRUE" "SIGNALS-ERROR" "SIGNALS-CONDITION"
           "SKIP" "EVAL-STR" "RUN-TESTS" "IMPLEMENTATION" "*EVAL-PACKAGE*"))

(in-package "CL3-TEST")

;;; ------------------------------------------------------------------
;;; Implementation identity
;;; ------------------------------------------------------------------

(defun implementation ()
  "Keyword naming the host Lisp, used to scope expected failures."
  #+sbcl  :sbcl
  #+clisp :clisp
  #+ecl   :ecl
  #+abcl  :abcl
  #+ccl   :ccl
  #-(or sbcl clisp ecl abcl ccl) :unknown)

;;; ------------------------------------------------------------------
;;; Registry
;;; ------------------------------------------------------------------

(defstruct (test (:conc-name test-))
  name finding phase severity xfail thunk)

(defvar *tests* '()
  "Registered tests, in definition order.")

(defun register-test (name finding phase severity xfail thunk)
  (let ((new (make-test :name name :finding finding :phase phase
                        :severity severity :xfail xfail :thunk thunk))
        (old (find name *tests* :key #'test-name)))
    (if old
        (setf *tests* (substitute new old *tests*))
        (setf *tests* (append *tests* (list new)))))
  name)

(defmacro deftest (name (&key finding phase severity xfail) &body body)
  "Define a regression test.  FINDING is the audit finding number; PHASE is a
task number, for acceptance tests that do not correspond to a finding.
SEVERITY is one of :critical :serious :minor.  XFAIL is either T or a list of
implementation keywords on which this test is currently expected to fail."
  `(register-test ',name ,finding ,phase ,severity ',xfail (lambda () ,@body)))

(defun test-label (test)
  (cond ((test-finding test) (format nil "F~2,'0d" (test-finding test)))
        ((test-phase test)   (format nil "~4@a" (test-phase test)))
        (t                   "    ")))

(defun xfail-here-p (xfail)
  (cond ((null xfail) nil)
        ((eq xfail t) t)
        ((listp xfail) (and (member (implementation) xfail) t))
        (t nil)))

;;; ------------------------------------------------------------------
;;; Assertions
;;; ------------------------------------------------------------------

(defvar *failures* '()
  "Assertion failure descriptions accumulated by the running test.")

(defun record-failure (text)
  (push text *failures*)
  nil)

(defun safely (thunk)
  "Run THUNK, returning either its value or a marker describing the condition
it signalled.  Assertions evaluate their forms through this so that one broken
assertion does not abandon the rest of the test."
  (handler-case (funcall thunk)
    (error (c) (list :signalled (type-of c) (princ-to-string c)))))

(defun signalled-p (v)
  (and (consp v) (eq (car v) :signalled)))

(defun describe-value (v)
  (if (signalled-p v)
      (format nil "~a: ~a" (second v) (third v))
      (format nil "~s" v)))

(defmacro is-equal (expected form &optional label)
  "Assert that FORM evaluates EQUAL to EXPECTED."
  (let ((e (gensym "EXPECTED")) (a (gensym "ACTUAL")))
    `(let ((,e ,expected)
           (,a (safely (lambda () ,form))))
       (unless (equal ,e ,a)
         (record-failure
          (format nil "~@[~a~%        ~]~s~%          expected: ~s~%          actual:   ~a"
                  ,label ',form ,e (describe-value ,a)))))))

(defmacro is-true (form &optional label)
  "Assert that FORM evaluates to a true value without signalling."
  (let ((a (gensym "ACTUAL")))
    `(let ((,a (safely (lambda () ,form))))
       (when (or (null ,a) (signalled-p ,a))
         (record-failure
          (format nil "~@[~a~%        ~]~s~%          expected: non-NIL~%          actual:   ~a"
                  ,label ',form (describe-value ,a)))))))

(defmacro signals-error (form &optional label)
  "Assert that FORM signals an ERROR."
  `(unless (handler-case (progn ,form nil) (error () t))
     (record-failure (format nil "~@[~a~%        ~]~s~%          expected: an ERROR"
                             ,label ',form))))

(defmacro signals-condition (type form &optional label)
  "Assert that FORM signals a condition of TYPE.  Works for non-serious
conditions such as STYLE-WARNING, which HANDLER-CASE on ERROR would miss."
  `(unless (block signalled
             (handler-bind ((,type (lambda (c) (declare (ignore c))
                                     (return-from signalled t))))
               (handler-case ,form (error () nil))
               nil))
     (record-failure (format nil "~@[~a~%        ~]~s~%          expected: a ~a"
                             ,label ',form ',type))))

;;; ------------------------------------------------------------------
;;; Skipping
;;; ------------------------------------------------------------------

(define-condition test-skipped ()
  ((reason :initarg :reason :reader skip-reason)))

(defun skip (reason)
  "Abandon the running test, recording it as skipped rather than failed."
  (signal 'test-skipped :reason reason))

;;; ------------------------------------------------------------------
;;; Evaluating Lisp1 source at run time
;;; ------------------------------------------------------------------

(defvar *eval-package* "COMMON-LISP-USER"
  "Package in which EVAL-STR reads and evaluates.  The suite points this at a
scratch package so tests cannot collide with the host environment.")

(defun eval-str (string)
  "READ and EVAL STRING at run time.

Most of the defects under test abort at *macroexpansion* time, so writing the
offending forms literally in this file would prevent the file itself from
loading.  Routing them through READ-FROM-STRING at run time keeps each failure
contained to its own test.  STRING must contain exactly one form; wrap several
in PROGN."
  (let ((*package* (find-package *eval-package*)))
    (eval (read-from-string string))))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defvar *capture-output* t
  "When true, discard whatever the code under test writes to the standard
streams.  These tests deliberately evaluate forms that break at macroexpansion
time, and the resulting compiler diagnostics would otherwise bury the report.
Bind to NIL when debugging a test.")

(defun run-one (test)
  "Run TEST, returning (values outcome failures skip-reason) where outcome is
one of :pass :fail :xfail :xpass :skip."
  (let ((*failures* '())
        (skipped nil)
        (sink (if *capture-output* (make-broadcast-stream) *standard-output*))
        (errsink (if *capture-output* (make-broadcast-stream) *error-output*)))
    (handler-case
        (let ((*standard-output* sink)
              (*error-output* errsink)
              (*trace-output* errsink))
          (funcall (test-thunk test)))
      (test-skipped (c) (setf skipped (skip-reason c)))
      (error (c)
        (record-failure (format nil "test body signalled ~a: ~a"
                                (type-of c) (princ-to-string c)))))
    (let* ((failures (reverse *failures*))
           (expected (xfail-here-p (test-xfail test)))
           (outcome (cond (skipped :skip)
                          ((and failures expected) :xfail)
                          (failures :fail)
                          (expected :xpass)
                          (t :pass))))
      (values outcome failures skipped))))

(defun outcome-label (outcome)
  (ecase outcome
    (:pass  "  pass ")
    (:fail  " FAIL  ")
    (:xfail " xfail ")
    (:xpass " XPASS ")
    (:skip  "  skip ")))

(defun run-tests (&key (stream *standard-output*))
  "Run every registered test.  Prints a report and returns the number of
unexpected results, which callers use as an exit code."
  (let ((counts (list :pass 0 :fail 0 :xfail 0 :xpass 0 :skip 0))
        (details '()))
    (format stream "~&~%Common Lisp 3 regression suite -- ~a ~a (~a)~%"
            (lisp-implementation-type) (lisp-implementation-version)
            (implementation))
    (format stream "~v,,,'-<~>~%" 72)
    (dolist (test (stable-sort (copy-list *tests*) #'<
                               :key (lambda (test) (or (test-finding test) 9999))))
      (multiple-value-bind (outcome failures skipped) (run-one test)
        (incf (getf counts outcome))
        (format stream "[~a] ~a  ~a~@[  (~(~a~))~]~@[  -- ~a~]~%"
                (outcome-label outcome)
                (test-label test)
                (string-downcase (symbol-name (test-name test)))
                (test-severity test)
                skipped)
        (when failures
          (push (list outcome test failures) details))))
    (setf details (reverse details))

    ;; Detail sections: known defects first, then anything unexpected.
    (flet ((section (title wanted)
             (let ((rows (remove-if-not (lambda (d) (eq (first d) wanted)) details)))
               (when rows
                 (format stream "~%~a~%~v,,,'-<~>~%" title 72)
                 (dolist (row rows)
                   (destructuring-bind (outcome test failures) row
                     (declare (ignore outcome))
                     (format stream "~%  ~a ~a~%" (test-label test)
                             (string-downcase (symbol-name (test-name test))))
                     (dolist (f failures)
                       (format stream "      ~a~%" f))))))))
      (section "KNOWN DEFECTS (expected to fail until fixed)" :xfail)
      (section "UNEXPECTED FAILURES" :fail))

    (let ((xpass (getf counts :xpass)))
      (when (plusp xpass)
        (format stream "~%~d test~:p marked :xfail now PASS.  A fix has landed --~%~
                          remove the :xfail marker so the test guards against regression.~%"
                xpass)))

    (format stream "~%~v,,,'-<~>~%" 72)
    (format stream "  ~d passed   ~d known defects   ~d skipped   ~d unexpected failures   ~d newly passing~%~%"
            (getf counts :pass) (getf counts :xfail) (getf counts :skip)
            (getf counts :fail) (getf counts :xpass))
    (finish-output stream)
    (getf counts :fail)))
