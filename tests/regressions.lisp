;;;; regressions.lisp -- one test per audit finding.
;;;;
;;;; One test per numbered audit finding, plus acceptance tests for the
;;;; behaviour each fix introduced.  Every test asserts the corrected behaviour.
;;;; A test labelled with a number rather than F<nn> is an acceptance test; the
;;;; number is the task it came from.
;;;;
;;;; All 22 findings are fixed, so no test carries an :xfail marker any more.
;;;; While the work was in progress each unfixed finding was marked, which is
;;;; what let the suite be written against the corrected behaviour from the
;;;; start and still run clean; findings 2 and 3 carried markers naming a
;;;; single implementation.  The mechanism is still in the harness for the next
;;;; time it is wanted.
;;;;
;;;; Two of these defects showed up on one implementation only, and two further
;;;; single-implementation defects turned up while fixing them, which is why the
;;;; suite runs on all five rather than on whichever is to hand.
;;;;
;;;; Lisp1 source under test is written as strings and evaluated through
;;;; EVAL-STR.  Several of these defects abort at macroexpansion time; written
;;;; literally they would stop this file from loading at all.

(in-package "CL3-TEST")

;;; Scratch package for the code under test, so nothing leaks into CL-USER.
(defpackage "CL3-USER" (:use "COMMON-LISP" "CL3"))
(setf *eval-package* "CL3-USER")

;;; A package that shadows a COMMON-LISP name, for finding 12.
(defpackage "CL3-TEST-SHADOW" (:use "COMMON-LISP") (:shadow "SECOND"))

(defparameter *test-dir*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*))
  "Directory holding this file; scratch files for the file-operation tests go
underneath it.")

(defun scratch-dir ()
  (let ((dir (merge-pathnames "scratch/" *test-dir*)))
    (ensure-directories-exist dir)
    dir))

(defun cl3-owns-p (name)
  "True when CL3 itself has a symbol called NAME -- not one inherited from
COMMON-LISP.  The distinction matters from Phase 4 on: with the shadowing
removed, (FIND-SYMBOL \"LOAD\" \"CL3\") finds CL:LOAD by inheritance, which
would otherwise read as the retired CL3:LOAD still being present."
  (and (member (nth-value 1 (find-symbol name "CL3")) '(:internal :external))
       t))

(defun cl3-symbol (name)
  "The function CL3 itself provides under NAME, or NIL.  Used by tests that
must survive the interface being renamed or trimmed."
  (let ((s (find-symbol name "CL3")))
    (and s (cl3-owns-p name) (fboundp s) s)))

(defun externalp (name)
  (eq :external (nth-value 1 (find-symbol name "CL3"))))

(defun warns-p (thunk)
  "True when THUNK signals a warning."
  (let ((warned nil))
    (handler-bind ((warning (lambda (c) (setq warned t) (muffle-warning c))))
      (ignore-errors (funcall thunk)))
    warned))

(defvar *compile-file-usable* :unknown)

(defun compile-file-usable-p ()
  "True when CL:COMPILE-FILE works at all here, tested on a file containing no
Lisp1 whatsoever.  ECL compiles via C, so a host C compiler newer than the ECL
headers takes its COMPILE-FILE out entirely -- a broken toolchain, not a
library defect, and the two should not be confused.

Answered once and remembered.  Re-probing would report a false negative on
CCL, which tracks duplicate definitions across COMPILE-FILE calls within a
session and counts one as a failure -- so a probe compiled twice under the
same function name fails the second time."
  (when (eq *compile-file-usable* :unknown)
    (setf *compile-file-usable*
          (let* ((dir (scratch-dir))
                 (*default-pathname-defaults* dir)
                 (probe (merge-pathnames "cf-probe.lisp" dir)))
            (with-open-file (s probe :direction :output :if-exists :supersede)
              (write-line "(defun lisp1-test-cf-probe (a) (+ a 1))" s))
            (handler-case
                (multiple-value-bind (output warnings failure)
                    (cl:compile-file probe :verbose nil :print nil)
                  (declare (ignore warnings))
                  (and output (not failure) t))
              (error () nil)))))
  *compile-file-usable*)


;;; ==================================================================
;;; Phase 1 -- lexical scoping
;;; ==================================================================

(deftest closures-are-lexical (:finding 1 :severity :critical)
  ;; Fixed in Phase 1.  DEFINE used to expand to DEFPARAMETER, which proclaimed
  ;; the name special globally and permanently; every later binding of that
  ;; name became dynamic, so closures captured nothing.  It now expands to a
  ;; hidden cell plus DEFINE-SYMBOL-MACRO, which a LET can shadow lexically.
  (eval-str "(progn (define f01-v 10)
                    (define f01-f (lambda (f01-v) (lambda () f01-v))))")
  (is-equal 1 (eval-str "(funcall (f01-f 1))")
            "a closure returns its own captured parameter, not the global")

  (eval-str "(progn
               (define f01-counter 0)
               (define f01-make
                 (lambda ()
                   (let ((f01-counter 0))
                     (lambda () (setq f01-counter (+ f01-counter 1)) f01-counter))))
               (define f01-c1 (f01-make))
               (define f01-c2 (f01-make)))")
  (is-equal 1 (eval-str "(f01-c1)")           "first counter, first call")
  (is-equal 2 (eval-str "(f01-c1)")           "first counter, second call")
  (is-equal 1 (eval-str "(f01-c2)")           "second counter is independent")
  (is-equal 0 (eval-str "f01-counter")        "the global was not clobbered"))


(deftest define-rejects-names-it-cannot-bind (:phase "1.3" :severity :serious)
  ;; DEFINE-SYMBOL-MACRO signals on an already-special symbol and on a
  ;; constant -- confirmed on SBCL, CCL and CLISP.  The diagnostic has to
  ;; explain the cause rather than let an opaque SIMPLE-PROGRAM-ERROR through,
  ;; because an image carrying pre-0.2 definitions hits this on every name.
  (eval-str "(defparameter p13-special 1)")
  (signals-error (eval-str "(define p13-special 2)")
                 "an already-special name is refused")
  (is-true (search "DEFINE-DYNAMIC"
                   (handler-case (progn (eval-str "(define p13-special 2)") "")
                     (error (c) (princ-to-string c))))
           "and the message names the alternative")
  (signals-error (eval-str "(define pi 3)")
                 "a constant is refused")
  (signals-error (eval-str "(define 42 3)")
                 "a non-symbol is refused"))

(deftest define-dynamic-is-dynamically-scoped (:phase "1.5" :severity :serious)
  ;; The escape hatch for code that genuinely wanted the pre-0.2 behaviour.
  (eval-str "(progn (define-dynamic p15-dyn 10)
                    (define p15-dyn-reader (lambda () p15-dyn)))")
  (is-equal 99 (eval-str "(let ((p15-dyn 99)) (p15-dyn-reader))")
            "rebinding a dynamic global is visible to a callee")
  ;; The contrast that Phase 1 exists to create.
  (eval-str "(progn (define p15-lex 10)
                    (define p15-lex-reader (lambda () p15-lex)))")
  (is-equal 10 (eval-str "(let ((p15-lex 99)) (p15-lex-reader))")
            "rebinding a lexical global is NOT visible to a callee")
  (is-equal 99 (eval-str "(let ((p15-lex 99)) p15-lex)")
            "but the local binding is visible where it is written"))

(deftest define-supports-redefinition-and-setq (:phase "1.6" :severity :serious)
  (eval-str "(define p16-v 1)")
  (is-equal 1 (eval-str "p16-v"))
  (eval-str "(define p16-v 2)")
  (is-equal 2 (eval-str "p16-v")     "DEFINE may be re-evaluated")
  (eval-str "(setq p16-v 3)")
  (is-equal 3 (eval-str "p16-v")     "SETQ reaches the value cell")
  (eval-str "(setf p16-v 4)")
  (is-equal 4 (eval-str "p16-v")     "so does SETF")
  (eval-str "(define p16-f (lambda (n) (* n 2)))")
  (is-equal 10 (eval-str "(p16-f 5)"))
  (eval-str "(define p16-f (lambda (n) (* n 3)))")
  (is-equal 15 (eval-str "(p16-f 5)") "a redefined function is seen at the call site")
  (eval-str "(setq p16-f (lambda (n) (* n 4)))")
  (is-equal 20 (eval-str "(p16-f 5)") "so is one installed by SETQ"))

(deftest definitions-survive-compile-and-load (:phase "1.2" :severity :serious)
  ;; Global lexicals put symbols from a private package into user fasls, so
  ;; the ordinary compile-and-load route has to keep working.
  (unless (compile-file-usable-p)
    (skip "CL:COMPILE-FILE unusable here, on a file with no Lisp1 in it"))
  (let* ((dir (scratch-dir))
         (*default-pathname-defaults* dir)
         (src (merge-pathnames "p12src.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(in-package \"CL3-USER\")" s)
      (write-line "(define p12-add (lambda (a b) (+ a b)))" s)
      (write-line "(define p12-use (lambda () (p12-add 3 4)))" s)
      (write-line "(define-meth p12-m ((a integer)) (* a 2))" s)
      (write-line "(define-meth p12-m ((a string)) :str)" s))
    (multiple-value-bind (output warnings failure)
        (handler-case (cl:compile-file src :verbose nil :print nil)
          (error (c) (values nil nil (princ-to-string c))))
      (is-true output          "the file compiles")
      (is-equal nil failure    "with no failure")
      (is-equal nil warnings   "and no warnings")
      (when output (cl:load output)))
    (is-equal 7      (eval-str "(p12-use)")   "a compiled definition runs")
    (is-equal 42     (eval-str "(p12-m 21)")  "a compiled method dispatches")
    (is-equal :str   (eval-str "(p12-m \"x\")"))))


;;; ==================================================================
;;; Phase 3 -- quasiquote
;;; ==================================================================

(deftest list-forms-are-converted (:finding 2 :severity :critical)
  ;; On CCL the backquote probe at lisp1.lisp:136 returns CL:LIST, so the guard
  ;; clause swallows every list construction and nothing inside is converted.
  (is-equal '(6)
            (eval-str "(lisp1 (let ((d (lambda (n) (* n 2)))) (list (d 3))))")
            "a function value called inside (LIST ...) is converted"))

(deftest splicing-macro-templates (:finding 3 :severity :critical)
  ;; ABCL read-expands backquote into a family of markers; only BACKQ-LIST is
  ;; recognised, so a splicing template becomes a funcall on BACKQ-APPEND.
  (eval-str "(define-macro f03-sum (&rest xs) `(+ ,@xs))")
  (is-equal 6 (eval-str "(f03-sum 1 2 3)")
            "a ,@ template expands correctly"))


;;; ==================================================================
;;; Phase 2 -- the converter
;;; ==================================================================

(deftest case-forms-are-converted (:finding 4 :severity :critical)
  ;; IS-BUILTIN calls (STRING x) on every head, so a numeric CASE key aborts
  ;; macroexpansion; a list key is treated as a call and silently mis-selects.
  (is-equal :one   (eval-str "(lisp1 (case 1 (1 :one) (2 :two)))")
            "numeric key does not abort macroexpansion")
  (is-equal :two   (eval-str "(lisp1 (case 2 (1 :one) (2 :two)))"))
  (is-equal :ab    (eval-str "(lisp1 (case 'a ((a b) :ab) (t :other)))")
            "a list of keys selects the right branch")
  (is-equal :two   (eval-str "(lisp1 (ecase 2 (1 :one) (2 :two)))")))

(deftest lexical-bindings-shadow-cl-names (:finding 5 :severity :critical)
  ;; IS-BUILTIN is consulted before anything else and knows nothing about
  ;; lexical scope, so a local function value named after a CL function is
  ;; ignored in favour of the builtin.  Silently wrong in the first case.
  (is-equal 42 (eval-str "(lisp1 (let ((list (lambda (n) (* n 2)))) (list 21)))")
            "a local binding named LIST shadows CL:LIST")
  (is-equal 42 (eval-str "(lisp1 (let ((count (lambda () 42))) (count)))")
            "a local binding named COUNT shadows CL:COUNT"))

(deftest flet-and-labels-survive-conversion (:finding 6 :severity :serious)
  ;; The binding list is walked as if it were a body of expressions, so each
  ;; definition spec is rewritten into a funcall and the form is malformed.
  (is-equal 1 (eval-str "(lisp1 (flet ((g (n) n)) (g 1)))"))
  (is-equal 0 (eval-str "(lisp1 (labels ((g (n) (if (= n 0) 0 (g (- n 1))))) (g 3)))")))

(deftest destructuring-patterns-survive-conversion (:finding 7 :severity :serious)
  (is-equal 3     (eval-str "(lisp1 (destructuring-bind (a b) '(1 2) (+ a b)))"))
  (is-equal '(1)  (eval-str "(lisp1 (loop for (a . b) in '((1 . 2)) collect a))")))

(deftest method-qualifiers-are-parsed (:finding 8 :severity :serious)
  ;; Both DEFINE-METHOD and the DEFMETHOD branch of CONVERT assume (CADDR X) is
  ;; the lambda list, so :before/:after/:around methods cannot be written.
  (is-equal 42
            (eval-str "(progn
                         (define-meth f08-m ((a integer)) (* a 2))
                         (define-meth f08-m :before ((a integer)) nil)
                         (f08-m 21))")
            "a :before qualifier is not mistaken for the lambda list"))

(deftest generic-functions-accept-many-methods (:finding 21 :severity :serious)
  ;; Found while carrying out Phase 1, not in the original audit; fixed there.
  ;; DEFINE-METHOD installs the call-dispatch macro in the symbol's function
  ;; cell, so DEFMETHOD could not add a second method to that name -- and the
  ;; failed attempt took the first method with it, leaving the generic function
  ;; undefined.  One method per generic function makes CLOS unusable.  Methods
  ;; now live on a hidden generic function held in the name's value cell.
  (eval-str "(define-meth f21-m ((a integer)) (* a 2))")
  (is-equal 42 (eval-str "(f21-m 21)")
            "the first method works")
  (is-true (eval-str "(define-meth f21-m ((a string)) :a-string)")
           "a second method can be added to the same generic function")
  (is-equal :a-string (eval-str "(f21-m \"x\")")
            "the second method dispatches")
  (is-equal 42 (eval-str "(f21-m 21)")
            "the first method still works afterwards"))

(deftest method-bodies-can-call-the-next-method (:finding 22 :severity :serious)
  ;; Found while writing the 0.2 examples.  CALL-NEXT-METHOD and NEXT-METHOD-P
  ;; are COMMON-LISP symbols that CLOS makes available inside a method body,
  ;; but they are not globally FBOUND -- so the converter, finding no function
  ;; behind the name, rewrote (call-next-method) into a reference to a
  ;; variable that does not exist.  They are now bound in the body's
  ;; environment, as CLOS binds them.
  (eval-str "(progn
               (define-meth f22-area ((s integer)) (* s s))
               (define-meth f22-area :around ((s integer))
                 (if (< s 0) 0 (call-next-method))))")
  (is-equal 25 (eval-str "(f22-area 5)")  "an :around method can call the next")
  (is-equal 0  (eval-str "(f22-area -5)") "and can decline to")
  (eval-str "(define-meth f22-np ((s integer)) (if (next-method-p) :more :last))")
  (is-equal :last (eval-str "(f22-np 1)") "NEXT-METHOD-P is available too"))

(deftest define-macro-survives-conversion (:finding 9 :severity :serious)
  ;; CONVERT does not recognise its own DEFINE-MACRO form, so the macro's
  ;; lambda list (a b) is rewritten to (funcall a b) -- three parameters.
  ;; This is the same code path CL3:COMPILE-FILE uses on every source file.
  (eval-str "(lisp1 (define-macro f09-add (a b) `(+ ,a ,b)))")
  (is-equal 7 (eval-str "(f09-add 3 4)")
            "the macro's lambda list is left intact"))

(deftest lambda-list-defaults-are-converted (:finding 13 :severity :serious)
  ;; The LAMBDA and DEFUN branches copy the lambda list verbatim, so an
  ;; &optional initialisation form never receives Lisp1 treatment.
  (is-equal 7
            (eval-str "(lisp1 (let ((h (lambda (n) (* n 3))))
                                ((lambda (a &optional (b (h 2))) (+ a b)) 1)))")
            "an &optional default may call a function held in a variable"))

(deftest defun-inside-lisp1-is-callable (:finding 14 :severity :serious)
  ;; CONVERT rewrites DEFUN to a bare DEFPARAMETER but, unlike DEFINE, never
  ;; installs the companion macro, so the name is callable only from inside
  ;; another converted form.
  (eval-str "(lisp1 (defun f14-fn (a) (* a 2)))")
  (is-equal 6 (eval-str "(f14-fn 3)")))

(deftest non-symbol-heads-do-not-signal (:finding 16 :severity :serious)
  ;; IS-BUILTIN calls (STRING x) and IS-MACRO calls (MACRO-FUNCTION x); both
  ;; require designators the converter does not check for.
  (is-equal :ab    (eval-str "(lisp1 (case #\\a ((#\\a #\\b) :ab) (t :other)))")
            "character keys in a list do not raise a type error")
  (is-equal :other (eval-str "(lisp1 (case \"x\" ((\"x\") :s) (t :other)))")
            "string keys do not raise a type error"))

(deftest builtin-test-uses-symbol-identity (:finding 12 :severity :serious)
  ;; Unit test of an internal predicate: IS-BUILTIN asks whether SOME symbol of
  ;; that name is external in COMMON-LISP, not whether this is that symbol.
  ;; IS-BUILTIN is kept as a named predicate, so this stays valid.
  (let ((pred (cl3-symbol "IS-BUILTIN")))
    (unless pred (skip "CL3::IS-BUILTIN not present"))
    (is-true  (funcall pred 'cl:second)
              "the real CL:SECOND is a builtin")
    (is-equal nil (funcall pred (intern "SECOND" "CL3-TEST-SHADOW"))
              "a shadowed symbol merely NAMED \"SECOND\" is not")))


(deftest nested-definitions-see-their-environment (:phase "2.1" :severity :serious)
  ;; Found after Phase 6, while checking what was left.  The converter handed
  ;; a definition form straight back for its own macro to expand, and that
  ;; macro converted with no environment -- so a definition written inside a
  ;; binding form did not see the binding.  Finding 5 again, one level in, and
  ;; silent: it returned (21) rather than 42.  Each definer is now factored
  ;; into an expander that takes the environment, used by both the macro and
  ;; the converter's walker.
  (is-equal 42 (eval-str "(progn (lisp1 (let ((list (lambda (n) (* n 2))))
                                          (define p21-nested (lambda (x) (list x)))))
                                 (p21-nested 21))")
            "a nested DEFINE sees a binding that shadows a CL name")
  (is-equal 42 (eval-str "(progn (lisp1 (let ((count (lambda (n) (* n 2))))
                                          (define-macro p21-nm (v) `(count ,v))))
                                 (lisp1 (let ((count (lambda (n) (* n 2))))
                                          (p21-nm 21))))")
            "so does a nested DEFINE-MACRO")
  (is-equal 42 (eval-str "(progn (lisp1 (let ((list (lambda (n) (* n 2))))
                                          (define-dynamic p21-nd (lambda (x) (list x)))))
                                 (p21-nd 21))")
            "and a nested DEFINE-DYNAMIC")
  ;; The ordinary case must be unaffected.
  (is-equal 42 (eval-str "(progn (lisp1 (let ((h (lambda (n) (* n 2))))
                                          (define p21-plain (lambda (x) (h x)))))
                                 (p21-plain 21))")
            "a name that collides with nothing still works"))

(deftest converter-is-quiet-on-ordinary-code (:phase "2.5" :severity :serious)
  ;; The unknown-macro default macroexpands and walks the expansion.  That is
  ;; correct by construction but must not be noisy, and must not drag
  ;; implementation internals into the output -- SBCL's LOOP expansion reaches
  ;; SB-KERNEL:UNALIGNED-DX-CONS, which is why LOOP has a walker of its own.
  (unless (compile-file-usable-p)
    (skip "CL:COMPILE-FILE unusable here, on a file with no Lisp1 in it"))
  (let* ((dir (scratch-dir))
         (*default-pathname-defaults* dir)
         (src (merge-pathnames "p25src.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(in-package \"CL3-USER\")" s)
      (write-line "(define p25-all" s)
      (write-line "  (lambda (xs)" s)
      (write-line "    (let* ((acc '()) (n 0))" s)
      (write-line "      (dolist (x xs) (push x acc) (incf n))" s)
      (write-line "      (flet ((twice (v) (* 2 v)))" s)
      (write-line "        (labels ((down (k) (if (= k 0) 0 (down (- k 1)))))" s)
      (write-line "          (list (loop for y in xs collect (twice y))" s)
      (write-line "                (loop for (a . b) in '((1 . 2)) collect (+ a b))" s)
      (write-line "                (case n (0 :none) ((1 2) :few) (t :many))" s)
      (write-line "                (destructuring-bind (p &optional (q (twice 5))) '(1) (+ p q))" s)
      (write-line "                (multiple-value-bind (d r) (floor 7 2) (+ d r))" s)
      (write-line "                (handler-case (error \"x\") (error (c) (declare (ignore c)) :caught))" s)
      (write-line "                (cond ((null acc) :empty) (t (down 3)))" s)
      (write-line "                (do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 3) s))" s)
      (write-line "                (with-output-to-string (o) (format o \"~a\" n))" s)
      (write-line "                (setf (car acc) (twice (car acc)))))))))" s))
    (multiple-value-bind (output warnings failure)
        (handler-case (cl:compile-file src :verbose nil :print nil)
          (error (c) (values nil (princ-to-string c) t)))
      (is-true  output        "a file using the breadth of CL compiles")
      (is-equal nil failure   "with no failure")
      (is-equal nil warnings  "and no warnings -- the converter stays quiet")
      (when output (cl:load output)))
    (is-equal '((2 4 6) (3) :many 11 4 :caught 0 3 "3" 6)
              (eval-str "(p25-all '(1 2 3))")
              "and every one of those forms behaves")))

(deftest user-macros-resolve-at-compile-time (:phase "2.1" :severity :serious)
  ;; A macro defined by DEFINE-MACRO and then used inside a converted body.
  ;; CCL registers a compile-time macro in the compilation environment only,
  ;; so without threading &ENVIRONMENT into the converter this compiles into a
  ;; call to a variable that does not exist.
  (unless (compile-file-usable-p)
    (skip "CL:COMPILE-FILE unusable here, on a file with no Lisp1 in it"))
  (let* ((dir (scratch-dir))
         (*default-pathname-defaults* dir)
         (src (merge-pathnames "p21src.lisp" dir)))
    (with-open-file (s src :direction :output :if-exists :supersede)
      (write-line "(in-package \"CL3-USER\")" s)
      (write-line "(define-macro p21-twice (x) `(* 2 ,x))" s)
      (write-line "(define p21-use (lambda (n) (p21-twice n)))" s))
    (multiple-value-bind (output warnings failure)
        (handler-case (cl:compile-file src :verbose nil :print nil)
          (error (c) (values nil (princ-to-string c) t)))
      (is-true  output       "the file compiles")
      (is-equal nil failure  "with no failure")
      (is-equal nil warnings "and no warnings")
      (when output (cl:load output)))
    (is-equal 42 (eval-str "(p21-use 21)") "and the macro was applied")))

(deftest backquote-is-handled-uniformly (:phase "2.5" :severity :serious)
  ;; Backquote representation is implementation-defined, and the old code
  ;; probed for a marker symbol -- which on CCL came back as CL:LIST, so no
  ;; (list ...) form anywhere was converted, and on ABCL missed the markers
  ;; that splicing and dotted templates use.  There is no probe now: a
  ;; quasiquote macro is expanded like any other, and a read-expanded template
  ;; is walked as the ordinary function calls it already is.
  (eval-str "(progn
               (define-macro p2q-plain  (a b)      `(+ ,a ,b))
               (define-macro p2q-splice (&rest xs) `(+ ,@xs))
               (define-macro p2q-dotted (a b)      `(+ ,a . (,b)))
               (define-macro p2q-nested (a)        `(list `(,',a))))")
  (is-equal 7  (eval-str "(p2q-plain 3 4)")     "a plain template")
  (is-equal 6  (eval-str "(p2q-splice 1 2 3)")  "a splicing template")
  (is-equal 7  (eval-str "(p2q-dotted 3 4)")    "a dotted template")
  (is-true     (eval-str "(p2q-nested zz)")     "a nested template")
  ;; The C10 case from the audit: a template naming a function that is only
  ;; lexically bound at the call site.  This never worked before -- the
  ;; template was left unconverted, so the call site saw a raw (dbl 21).
  (eval-str "(define-macro p2q-apply (f v) `(,f ,v))")
  (is-equal 42 (eval-str "(lisp1 (let ((dbl (lambda (n) (* n 2)))) (p2q-apply dbl 21)))")
            "a macro template can name a lexically bound function"))

(deftest form-walkers-are-extensible (:phase "2.3" :severity :serious)
  ;; The public escape hatch: a macro of your own whose arguments are not all
  ;; expressions, which you would rather the converter did not macroexpand.
  (let ((dfw (find-symbol "DEFINE-FORM-WALKER" "CL3")))
    (is-true (and dfw (macro-function dfw)) "CL3:DEFINE-FORM-WALKER exists")
    (is-true (externalp "DEFINE-FORM-WALKER") "and is exported")
    (is-true (externalp "CONVERT") "CL3:CONVERT is exported for walkers to call"))
  ;; A form whose second subform is a literal key list, not an expression.
  (eval-str "(progn
               (defmacro p23-pick (keys value) `(if (member ,value ',keys) :hit :miss))
               (cl3:define-form-walker p23-pick (form env)
                 (list (first form) (second form) (cl3:convert (third form) env))))")
  (is-equal :hit (eval-str "(lisp1 (let ((k 2)) (p23-pick (1 2 3) k)))")
            "the registered walker leaves the key list alone"))

(deftest unknown-macro-expansion-can-be-disabled (:phase "2.5" :severity :minor)
  (is-true (find-symbol "*EXPAND-UNKNOWN-MACROS*" "CL3")
           "the switch exists")
  ;; With expansion off, an unregistered macro is left alone and reported
  ;; rather than being rewritten on a guess.
  (let ((switch (find-symbol "*EXPAND-UNKNOWN-MACROS*" "CL3")))
    (progv (list switch) (list nil)
      (signals-condition style-warning
                         (eval-str "(lisp1 (with-open-file (s \"/dev/null\") s))")
                         "and leaving a form alone is reported, not silent"))))


;;; ------------------------------------------------------------------
;;; Documented limitations.  These are not defects; they are the places
;;; where Lisp1 declines to convert, and README.txt says so.  Pinned here
;;; so the behaviour stays deliberate rather than becoming folklore.
;;; ------------------------------------------------------------------

(deftest macrolet-macros-are-left-alone-loudly (:phase "2.5" :severity :minor)
  ;; A local macro's shape is unknowable here -- the converter runs before
  ;; Common Lisp establishes the MACROLET, so the expansion cannot be had.
  ;; Leaving the call alone is the safe answer, and saying so is the point:
  ;; it marks a spot where Lisp1 semantics do not reach.
  (signals-condition style-warning
                     (eval-str "(lisp1 (macrolet ((m (x) `(list ,x))) (m 1)))")
                     "a MACROLET macro call is reported")
  (is-equal '(1) (eval-str "(lisp1 (macrolet ((m (x) `(list ,x))) (m 1)))")
            "and the form still means what Common Lisp says it means")
  ;; The definitions themselves are ordinary code and are converted.
  (is-equal 42 (eval-str "(lisp1 (let ((dbl (lambda (n) (* n 2))))
                                   (macrolet ((m (x) (list 'funcall 'dbl x)))
                                     (m 21))))")
            "a MACROLET body still sees the surrounding lexical environment"))

(deftest unconverted-warnings-can-be-silenced (:phase "2.5" :severity :minor)
  (is-true (find-symbol "*WARN-ON-UNCONVERTED*" "CL3") "the switch exists")
  (let ((switch (find-symbol "*WARN-ON-UNCONVERTED*" "CL3")))
    (progv (list switch) (list nil)
      (is-equal nil
                (warns-p (lambda ()
                           (eval-str "(lisp1 (macrolet ((m (x) `(list ,x))) (m 1)))")))
                "and *WARN-ON-UNCONVERTED* NIL silences the report"))))

(deftest conversion-reaches-only-where-lisp1-can-see (:phase "6.3" :severity :minor)
  ;; The boundary README.txt describes: DEFINE, DEFINE-MACRO, DEFINE-METHOD,
  ;; LISP1, EVAL-FORM and REP convert what they are given.  Code reached any
  ;; other way is ordinary Common Lisp, and a bare top level expression over a
  ;; local function value is a plain undefined-function call.
  (signals-error (eval-str "(let ((f (lambda (n) (* n 2)))) (f 21))")
                 "an unwrapped expression is ordinary CL, and says so")
  (is-equal 42 (eval-str "(lisp1 (let ((f (lambda (n) (* n 2)))) (f 21)))")
            "wrapping it in LISP1 is the fix")
  ;; Calls to defined names need no wrapping, which is why plain CL:LOAD works.
  (eval-str "(define p63-dbl (lambda (n) (* n 2)))")
  (is-equal 42 (eval-str "(p63-dbl 21)")
            "a call to a defined name needs no wrapping"))


;;; ==================================================================
;;; Phase 4 -- file operations and package surface
;;; ==================================================================

(deftest compile-file-is-robust (:finding 10 :severity :serious)
  ;; Phase 4 retired CL3:COMPILE-FILE rather than repairing it, so the
  ;; supported route is stock CL:COMPILE-FILE -- covered by tests 1.2 and 2.5.
  ;; What is asserted here is that nothing in the library still writes an
  ;; intermediate file into the current directory or swallows a failed compile.
  ;; If the decision is ever reversed, the original robustness assertions run
  ;; against whatever replaces it.
  (let ((cf (cl3-symbol "COMPILE-FILE")))
    (cond
      ((null cf)
       (is-equal nil (cl3-owns-p "COMPILE-FILE")
                 "no CL3:COMPILE-FILE, not even an unexported leftover")
       (unless (compile-file-usable-p)
         (skip "CL:COMPILE-FILE unusable here, on a file with no Lisp1 in it"))
       (let* ((dir (scratch-dir))
              (*default-pathname-defaults* dir))
         (ignore-errors (delete-file (merge-pathnames "tmp.lisp" dir)))
         (with-open-file (s (merge-pathnames "f10.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(in-package \"CL3-USER\")" s)
           (write-line "(define f10-fn (lambda (a) (* a 3)))" s))
         ;; The printer control variables that used to corrupt the output.
         (multiple-value-bind (output warnings failure)
             (let ((*print-length* 3) (*print-level* 2) (*print-base* 16))
               (cl:compile-file (merge-pathnames "f10.lisp" dir)
                                :verbose nil :print nil))
           (declare (ignore warnings))
           (is-true output       "compiling is unaffected by *PRINT-LENGTH* etc.")
           (is-equal nil failure "and reports no failure")
           (when output (cl:load output)))
         (is-equal 63 (eval-str "(f10-fn 21)")
                   "and the compiled definition is intact, not truncated")
         (is-equal nil (probe-file (merge-pathnames "tmp.lisp" dir))
                   "no intermediate file is left in the current directory")))
      (t
       ;; Kept after all: it must then actually be robust.
       (unless (compile-file-usable-p)
         (skip "CL:COMPILE-FILE unusable here, on a file with no Lisp1 in it"))
       (let* ((dir (scratch-dir))
              (*default-pathname-defaults* dir))
         (with-open-file (s (merge-pathnames "f10a.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(defparameter *f10-data* '(1 2 3 4 5 6 7 8 9 10))" s))
         (let ((*print-length* 3)) (ignore-errors (funcall cf "f10a")))
         (is-true (probe-file (merge-pathnames "f10a.fasl" dir))
                  "a fasl is produced even when *PRINT-LENGTH* is bound")
         (with-open-file (s (merge-pathnames "tmp.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line ";; a file the user already had" s))
         (with-open-file (s (merge-pathnames "f10b.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(defparameter *f10b* 1)" s))
         (is-true (handler-case (progn (funcall cf "f10b") t) (error () nil))
                  "compiling works when ./tmp.lisp already exists")
         (is-true (probe-file (merge-pathnames "tmp.lisp" dir))
                  "the user's own tmp.lisp survives")
         (with-open-file (s (merge-pathnames "f10c.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(defun f10-bad (1 2) 3)" s))
         (is-true (nth-value 2 (ignore-errors (funcall cf "f10c")))
                  "a failed compile reports FAILURE-P"))))))

(deftest load-preserves-environment (:finding 11 :severity :serious)
  ;; As above: CL3:LOAD is retired, so CL:LOAD is the supported route and
  ;; the point is that it handles a Lisp1 file without the two defects the
  ;; old one had -- leaking IN-PACKAGE into the caller, and stopping early and
  ;; silently on a file containing its EOF sentinel.
  (let ((ld (cl3-symbol "LOAD")))
    (cond
      ((null ld)
       (is-equal nil (cl3-owns-p "LOAD")
                 "no CL3:LOAD, not even an unexported leftover")
       (let* ((dir (scratch-dir))
              (*default-pathname-defaults* dir)
              (src (merge-pathnames "f11.lisp" dir)))
         (with-open-file (s src :direction :output :if-exists :supersede)
           (write-line "(defpackage \"CL3-TEST-LEAK\" (:use \"COMMON-LISP\"))" s)
           (write-line "(in-package \"CL3-TEST-LEAK\")" s)
           ;; the symbol that used to be the EOF sentinel, here as ordinary
           ;; data -- the old loader stopped dead at it, without a word
           (write-line "(defparameter *f11-marker* 'cl3::eof)" s)
           (write-line "(in-package \"CL3-USER\")" s)
           (write-line "(cl3:define f11-fn (lambda () :loaded))" s))
         (let ((before *package*))
           (cl:load src)
           (is-equal (package-name before) (package-name *package*)
                     "an IN-PACKAGE in the loaded file does not leak to the caller"))
         (is-equal :loaded (eval-str "(f11-fn)")
                   "and a file containing CL3::EOF loads to the end")))
      (t
       (let* ((dir (scratch-dir))
              (*default-pathname-defaults* dir))
         (with-open-file (s (merge-pathnames "f11a.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(defpackage \"CL3-TEST-LEAK2\" (:use \"COMMON-LISP\"))" s)
           (write-line "(in-package \"CL3-TEST-LEAK2\")" s))
         (let ((before *package*))
           (unwind-protect
                (ignore-errors (funcall ld (merge-pathnames "f11a.lisp" dir)))
             (setf *package* before))
           (is-equal (package-name before) (package-name *package*)
                     "an IN-PACKAGE in the loaded file does not leak"))
         (with-open-file (s (merge-pathnames "f11b.lisp" dir)
                            :direction :output :if-exists :supersede)
           (write-line "(defparameter *f11-first* 1)" s)
           (write-line "cl3::eof" s)
           (write-line "(defparameter *f11-second* 2)" s))
         (ignore-errors (funcall ld (merge-pathnames "f11b.lisp" dir)))
         (is-true (boundp (intern "*F11-SECOND*" "CL3-USER"))
                  "a file containing CL3::EOF loads to the end"))))))

(deftest package-surface-is-clean (:finding 17 :severity :minor)
  ;; The package shadows LOAD, COMPILE-FILE and EVAL but exports none of them,
  ;; so the shadowing accomplishes nothing -- and exporting them as they stand
  ;; would break every CL:LOAD call in a package that used CL3.
  (is-equal nil (package-shadowing-symbols "CL3")
            "no COMMON-LISP symbols are shadowed")
  (dolist (name '("DEFINE" "DEFINE-MACRO" "DEFINE-METH" "LISP1" "REP"))
    (is-true (externalp name) (format nil "CL3:~a is exported" name))))


(deftest using-lisp1-does-not-disturb-cl (:phase "4.1" :severity :serious)
  ;; The point of removing the shadows: a package that uses CL3 must still
  ;; get CL:LOAD, CL:COMPILE-FILE and CL:EVAL, unshadowed and unsurprising.
  (dolist (name '("LOAD" "COMPILE-FILE" "EVAL"))
    (is-equal (find-symbol name "COMMON-LISP") (find-symbol name "CL3-USER")
              (format nil "~a in a package using CL3 is still CL:~a" name name)))
  (is-equal 3 (eval-str "(eval '(+ 1 2))")
            "CL:EVAL reached through a package that uses CL3")
  (is-true (eval-str "(fboundp 'load)")
           "CL:LOAD is intact"))

(deftest public-api-is-exported (:phase "4.2" :severity :serious)
  (dolist (name '("DEFINE" "DEFINE-DYNAMIC" "DEFINE-MACRO" "DEFINE-METHOD"
                  "LISP1" "CONVERT" "EVAL-FORM" "DEFINE-FORM-WALKER"
                  "*EXPAND-UNKNOWN-MACROS*" "*WARN-ON-UNCONVERTED*"
                  "UNCONVERTED-FORM" "REP"))
    (is-true (externalp name) (format nil "CL3:~a is exported" name)))
  ;; Nothing exported may collide with anything already accessible where the
  ;; user will (use-package "CL3").  Checking only against COMMON-LISP is
  ;; not enough: CL-USER inherits from implementation packages too, and
  ;; CL3:TRACE-FUNCTION -- as it was briefly named -- collided with
  ;; CCL:TRACE-FUNCTION and broke (use-package "CL3") on CCL alone.
  (dolist (target '("COMMON-LISP" "COMMON-LISP-USER"))
    (let ((clashes '()))
      (do-external-symbols (sym "CL3")
        (let ((other (find-symbol (symbol-name sym) target)))
          (when (and other (not (eq other sym)))
            (push (symbol-name sym) clashes))))
      (is-equal nil clashes
                (format nil "no exported symbol collides with anything in ~a" target))))
  ;; The documented way in must actually work.
  (is-true (handler-case
               (let ((p (or (find-package "CL3-USE-TEST")
                            (make-package "CL3-USE-TEST" :use '("COMMON-LISP")))))
                 (use-package "CL3" p)
                 (delete-package p)
                 t)
             (error () nil))
           "(use-package \"CL3\") signals no conflict"))

(deftest eval-form-replaces-the-shadowed-eval (:phase "4.2" :severity :serious)
  ;; What CL3:EVAL did, under a name that does not shadow anything.
  (eval-str "(define p42-triple (lambda (n) (* n 3)))")
  (is-equal 63 (eval-str "(cl3:eval-form '(p42-triple 21))")
            "EVAL-FORM converts before evaluating")
  (is-equal 42 (eval-str "(cl3:eval-form '(let ((f (lambda (n) (* n 2)))) (f 21)))")
            "including a form CL:EVAL alone could not run")
  ;; The capability the retired loader had, and where it went: an unwrapped
  ;; top level expression using a lexically bound function value needs LISP1.
  (is-equal 42 (eval-str "(lisp1 (let ((f (lambda (n) (* n 2)))) (f 21)))")
            "LISP1 wraps what the retired loader used to convert file-wide"))

;;; ==================================================================
;;; Phase 5 -- REPL
;;; ==================================================================

(deftest repl-is-robust (:finding 15 :severity :serious)
  (let ((rep (cl3-symbol "REP")))
    (unless rep (skip "CL3::REP not present"))
    (flet ((feed (input)
             (with-input-from-string (in input)
               (let ((*standard-input* in)
                     (*standard-output* (make-broadcast-stream)))
                 (handler-case (progn (funcall rep) :ok)
                   (error (c) (list :error (type-of c))))))))
      (is-equal :ok (feed "")               "end of input exits cleanly")
      (is-equal :ok (feed "quit")           "a bare QUIT exits")
      (is-equal :ok (feed "exit")           "a bare EXIT exits")
      (is-equal :ok (feed "(quit)")         "(QUIT) still exits")
      (is-equal :ok (feed "(+ 1 2)")        "a form is evaluated, then EOF exits")
      (is-equal :ok (feed "(1 2)")          "a non-symbol head is reported, loop continues")
      (is-equal :ok (feed "(no-such-fn 1)") "an eval error is reported, loop continues"))))

(deftest portable-exit-exists (:finding 18 :severity :minor)
  ;; repq had only #+:sbcl and #+:clisp branches, so on ECL, ABCL and CCL it
  ;; returned without exiting -- three of the four Lisps the README claims.
  ;; Deliberately not called here: it would terminate the test run.  That it
  ;; exits, and with the right status, is verified by running it directly.
  (is-true (cl3-symbol "QUIT-LISP")    "CL3:QUIT-LISP exists")
  (is-true (externalp "QUIT-LISP")       "CL3:QUIT-LISP is exported"))


(defun rep-session (text)
  "Run REP over TEXT and return everything it printed."
  (let ((rep (cl3-symbol "REP"))
        (out (make-string-output-stream)))
    (with-input-from-string (in text)
      (let ((*package* (find-package "CL3-USER")))
        (funcall rep :input in :output out)))
    (get-output-stream-string out)))

(deftest repl-continues-after-an-error (:phase "5.4" :severity :serious)
  ;; The old loop unwound out of itself on the first error, so one typo ended
  ;; the session.
  (let ((text (rep-session "(no-such-fn 1) (1 2) (define p54 41) (+ p54 1)")))
    (is-true (search "42" text)
             "a form after two bad ones is still evaluated")
    (is-true (search "; error" text)
             "and the errors were reported rather than swallowed")))

(deftest repl-keeps-history-variables (:phase "5.5" :severity :minor)
  (let ((text (rep-session "(+ 1 2) (* * 10)")))
    (is-true (search "30" text) "* holds the previous result"))
  (let ((text (rep-session "(list 1 2) (length +)")))
    ;; + is the previous FORM, (list 1 2), whose length is 3
    (is-true (search "3" text) "+ holds the previous form")))

(deftest repl-prints-all-values (:phase "5.1" :severity :minor)
  (let ((text (rep-session "(floor 7 2)")))
    (is-true (and (search "3" text) (search "1" text))
             "both values of a multiple-value form are printed"))
  (let ((text (rep-session "(values)")))
    (is-true (search "no value" text)
             "a form returning nothing says so")))

(deftest repl-applies-lisp1-semantics (:phase "5.1" :severity :serious)
  ;; The reason REP exists rather than deferring to the host REPL: it converts
  ;; each form, so a bare top level expression over a local function value
  ;; works without being wrapped in LISP1.
  (let ((text (rep-session "(let ((f (lambda (n) (* n 2)))) (f 21))")))
    (is-true (search "42" text)
             "an unwrapped top level form gets Lisp1 semantics")))

;;; ==================================================================
;;; Phase 6 -- interop and arity checking
;;; ==================================================================

(deftest defined-functions-reachable-from-cl (:finding 19 :severity :minor)
  (eval-str "(define f19-dbl (lambda (n) (* n 2)))")
  ;; Already true today; pinned so Phase 1 cannot regress it.
  (is-equal '(2 4 6) (eval-str "(mapcar f19-dbl '(1 2 3))")
            "a defined function can be passed to a CL higher-order function")
  ;; #'name and CL:TRACE cannot work by design -- a symbol cannot hold both a
  ;; macro and a function -- so accessors are the supported route.
  (is-true (cl3-symbol "FUNCTION-OF") "CL3:FUNCTION-OF exists"))

(deftest definitions-are-reachable-and-traceable (:phase "6.1" :severity :serious)
  (eval-str "(define p61-dbl (lambda (n) (* n 2)))")
  (eval-str "(define p61-var 41)")
  ;; The accessors.
  (is-true  (functionp (eval-str "(cl3:function-of 'p61-dbl)"))
            "FUNCTION-OF gives the function object")
  (is-equal 84 (eval-str "(funcall (cl3:function-of 'p61-dbl) 42)")
            "which can be called")
  (is-equal 41 (eval-str "(cl3:value-of 'p61-var)")
            "VALUE-OF gives whatever the name holds")
  (signals-error (eval-str "(cl3:function-of 'p61-var)")
                 "FUNCTION-OF refuses a name holding a non-function")
  (signals-error (eval-str "(cl3:value-of 'p61-never-defined)")
                 "and an undefined name is an error, not an obscure one")
  (eval-str "(setf (cl3:value-of 'p61-var) 99)")
  (is-equal 99 (eval-str "p61-var") "VALUE-OF is settable")
  ;; What already worked, pinned: plain CL reaches the value through the name.
  (is-equal '(2 4 6) (eval-str "(mapcar p61-dbl '(1 2 3))")
            "a defined function can be passed to a CL higher-order function")
  ;; Tracing.
  (let ((text (with-output-to-string (out)
                (let ((*trace-output* out))
                  (eval-str "(cl3:trace-calls 'p61-dbl)")
                  (eval-str "(p61-dbl 21)")))))
    (is-true (search "P61-DBL" text :test #'char-equal)
             "TRACE-CALLS reports the call"))
  (is-equal 42 (eval-str "(p61-dbl 21)") "and the traced function still works")
  (eval-str "(cl3:untrace-calls 'p61-dbl)")
  (let ((text (with-output-to-string (out)
                (let ((*trace-output* out))
                  (eval-str "(p61-dbl 21)")))))
    (is-equal "" text "UNTRACE-CALLS stops the reporting"))
  (is-equal 42 (eval-str "(p61-dbl 21)") "and restores the original function"))

(deftest arity-checking-is-accurate (:phase "6.2" :severity :serious)
  (eval-str "(progn (define p62-two  (lambda (a b) (+ a b)))
                    (define p62-opt  (lambda (a &optional b) (list a b)))
                    (define p62-rest (lambda (a &rest more) (list a more))))")
  (flet ((expand (text) (lambda () (eval-str text))))
    (is-true  (warns-p (expand "(macroexpand-1 '(p62-two 1))"))
              "too few arguments warns")
    (is-true  (warns-p (expand "(macroexpand-1 '(p62-two 1 2 3))"))
              "too many arguments warns")
    (is-equal nil (warns-p (expand "(macroexpand-1 '(p62-two 1 2))"))
              "the right number does not")
    (is-equal nil (warns-p (expand "(macroexpand-1 '(p62-opt 1))"))
              "&optional may be omitted")
    (is-equal nil (warns-p (expand "(macroexpand-1 '(p62-opt 1 2))"))
              "or supplied")
    (is-true  (warns-p (expand "(macroexpand-1 '(p62-opt 1 2 3))"))
              "but not exceeded")
    (is-equal nil (warns-p (expand "(macroexpand-1 '(p62-rest 1 2 3 4))"))
              "&rest is unbounded")
    (is-true  (warns-p (expand "(macroexpand-1 '(p62-rest))"))
              "though its required argument is still required"))
  ;; A style warning, not an error: the name holds a variable and may be
  ;; reassigned to a function of another arity at run time.
  (is-true (eval-str "(macroexpand-1 '(p62-two 1 2 3))")
           "a mis-arity call still expands")
  (signals-condition style-warning
                     (eval-str "(macroexpand-1 '(p62-two 1 2 3))")
                     "and what it signals is a STYLE-WARNING")
  ;; Redefining with a different arity updates the check.
  (eval-str "(define p62-two (lambda (a b c) (+ a b c)))")
  (is-equal nil (warns-p (lambda () (eval-str "(macroexpand-1 '(p62-two 1 2 3))")))
            "redefinition updates the expected arity")
  ;; And it can be switched off.
  (let ((switch (find-symbol "*CHECK-ARITY*" "CL3")))
    (is-true switch "CL3:*CHECK-ARITY* exists")
    (progv (list switch) (list nil)
      (is-equal nil (warns-p (lambda () (eval-str "(macroexpand-1 '(p62-two 1))")))
                "and *CHECK-ARITY* NIL silences it"))))

(deftest arity-is-checked-at-expansion (:finding 20 :severity :minor)
  ;; When DEFINE's value form is a literal lambda its arity is known at
  ;; macroexpansion time, so a mis-arity call should warn then rather than
  ;; failing at run time.  A style warning, not an error: the variable may
  ;; legally be reassigned to a function of different arity.
  (eval-str "(define f20-add (lambda (a b) (+ a b)))")
  (signals-condition style-warning
                     (eval-str "(macroexpand-1 '(f20-add 1 2 3))")
                     "too many arguments warns at macroexpansion time"))


;;; ==================================================================
;;; Common Lisp 3 -- the parts that are not the single namespace
;;; ==================================================================

(deftest symbols-are-case-sensitive (:phase "cl3" :severity :serious)
  ;; cl3.lisp sets (readtable-case *readtable*) to :invert, so VAR1 and var1
  ;; are different symbols.  Global lexicals key their hidden cell on the
  ;; symbol name, so two names differing only in case must not share a cell.
  ;; Under :invert an all-lower-case token reads as upper case and an
  ;; all-upper-case one reads as lower case, so var1 and VAR1 are the symbols
  ;; named "VAR1" and "var1" -- two variables, not one.  Names are compared
  ;; rather than symbols, because the test package and the package under test
  ;; are different.
  (eval-str "(progn (define var1 'one) (define VAR1 'two))")
  (is-equal "ONE" (symbol-name (eval-str "var1"))
            "var1 holds its own value")
  (is-equal "TWO" (symbol-name (eval-str "VAR1"))
            "and VAR1 is a different variable holding another")
  (is-true (not (eq (eval-str "var1") (eval-str "VAR1")))
           "the two are genuinely distinct")
  (eval-str "(progn (define abc 66) (define ABC (lambda (a b) (+ a b))))")
  (is-equal 66 (eval-str "abc")      "a value and a function may differ only in case")
  (is-equal 11 (eval-str "(ABC 5 6)"))
  (eval-str "(define xx ABC)")
  (is-equal 11 (eval-str "(xx 5 6)") "and the function is assignable as a value"))

(deftest clos-layer-works (:phase "cl3" :severity :serious)
  ;; DEFINE-CLASS builds a class and a parallel class-object holding the
  ;; class variables; GET-SLOT and SET-SLOT reach either.
  (eval-str "(define-class cls-a () (cv1 cv2) (iv1 iv2))")
  (eval-str "(defparameter inst-a (make-instance cls-a))")
  (is-equal 44 (eval-str "(set-slot cls-a 'cv1 44)")   "a class variable can be set")
  (is-equal 77 (eval-str "(set-slot inst-a 'iv1 77)")  "and an instance variable")
  (is-equal 44 (eval-str "(get-slot cls-a 'cv1)")      "and read back")
  (is-equal 77 (eval-str "(get-slot inst-a 'iv1)"))
  ;; A class variable is one storage location shared with every subclass.
  (eval-str "(define-class cls-b (cls-a) (cv3) (iv3))")
  (eval-str "(defparameter inst-b (make-instance cls-b))")
  (is-equal 44 (eval-str "(get-slot cls-b 'cv1)")
            "a subclass sees the value its parent set")
  (is-equal 42 (eval-str "(progn (set-slot cls-b 'cv1 42) (get-slot cls-a 'cv1))")
            "and setting it through the subclass sets the one shared value")
  ;; Declared lower down, it is not visible above.
  (eval-str "(set-slot cls-b 'cv3 7)")
  (signals-error (eval-str "(get-slot cls-a 'cv3)")
                 "a class variable is not visible above the class declaring it"))

(deftest class-variables-are-shared-like-smalltalk (:phase "cl3" :severity :serious)
  ;; The slots holding class variables are :allocation :class, so there is one
  ;; storage location per declaring class rather than one per class in the
  ;; hierarchy.  Each class used to get its own copy: the shape was inherited,
  ;; the value was not.
  (eval-str "(progn (define-class sv-1 () (cv1 cv2) (iv1))
                    (define-class sv-2 (sv-1) () (iv2))
                    (define-class sv-3 (sv-2) (cv3) (iv3)))")
  (is-equal '(42 42 42)
            (eval-str "(progn (set-slot sv-1 'cv1 42)
                              (list (get-slot sv-1 'cv1) (get-slot sv-2 'cv1) (get-slot sv-3 'cv1)))")
            "set on the declaring class, seen by every subclass")
  (is-equal '(99 99 99)
            (eval-str "(progn (set-slot sv-3 'cv1 99)
                              (list (get-slot sv-1 'cv1) (get-slot sv-2 'cv1) (get-slot sv-3 'cv1)))")
            "and set through a subclass, seen by the declaring class")
  ;; Diamond inheritance still finds one location.
  (eval-str "(progn (define-class sv-top () (tv) ()) (define-class sv-l (sv-top) () ())
                    (define-class sv-r (sv-top) () ())  (define-class sv-bot (sv-l sv-r) () ()))")
  (is-equal '(3 3 3 3)
            (eval-str "(progn (set-slot sv-bot 'tv 3)
                              (list (get-slot sv-top 'tv) (get-slot sv-l 'tv)
                                    (get-slot sv-r 'tv) (get-slot sv-bot 'tv)))")
            "a diamond shares one location, not two")
  (is-equal '(5 5)
            (eval-str "(progn (define-class sv-d () ((cvd :initform 5)) ())
                              (define-class sv-e (sv-d) () ())
                              (list (get-slot sv-d 'cvd) (get-slot sv-e 'cvd)))")
            ":initform on a class variable is shared too"))

(deftest instances-reach-class-variables (:phase "cl3" :severity :serious)
  ;; As in Smalltalk, a method can read and write its class variables through
  ;; SELF.  An instance variable of the same name shadows the class variable.
  (eval-str "(progn (define-class iv-1 () (cv) (iv))
                    (define-class iv-2 (iv-1) () ())
                    (defparameter iv-iu (make-instance iv-2))
                    (set-slot iv-1 'cv 42))")
  (is-equal 42 (eval-str "(get-slot iv-iu 'cv)")
            "an instance reads a class variable")
  (is-equal '(7 7) (eval-str "(progn (set-slot iv-iu 'cv 7)
                                     (list (get-slot iv-1 'cv) (get-slot iv-iu 'cv)))")
            "and writing through the instance sets the shared value")
  (is-equal 3 (eval-str "(progn (set-slot iv-iu 'iv 3) (get-slot iv-iu 'iv))")
            "an instance variable still takes precedence")
  (signals-error (eval-str "(get-slot iv-iu 'no-such-slot)")
                 "a name that is neither is still an error")
  (is-equal 15 (eval-str "(progn (define-method bump iv-2 (n)
                                   (set-slot self 'cv (+ (get-slot self 'cv) n)))
                                 (bump iv-iu 8)
                                 (get-slot iv-1 'cv))")
            "a method reaches the class variable through SELF"))

(deftest clos-methods-bind-self-in-any-package (:phase "cl3" :severity :serious)
  ;; DEFINE-METHOD binds the instance to SELF.  Unexported, that anaphor is a
  ;; symbol in the CL3 package, so a method body written anywhere else refers
  ;; to its own SELF and the method fails with an unbound variable -- which is
  ;; every user of the system, since nobody writes their code in CL3.
  (is-true (eq :external (nth-value 1 (find-symbol "SELF" "CL3")))
           "CL3:SELF is exported")
  (eval-str "(define-class cls-c () (cv) (iv))")
  (eval-str "(defparameter inst-c (make-instance cls-c))")
  (eval-str "(set-slot inst-c 'iv 77)")
  (eval-str "(define-method addv cls-c (val) (+ (get-slot self 'iv) val))")
  (is-equal 177 (eval-str "(addv inst-c 100)")
            "a method body in another package can name SELF")
  ;; Class methods dispatch on the class object itself.
  (eval-str "(set-slot cls-c 'cv 44)")
  (eval-str "(defmethod addc ((cls (eql cls-c)) val) (+ (get-slot cls 'cv) val))")
  (is-equal 144 (eval-str "(addc cls-c 100)") "and a class method still works"))

(deftest clos-forms-survive-conversion (:phase "cl3" :severity :serious)
  ;; DEFINE-CLASS and DEFINE-METHOD expand to DEFCLASS and DEFMETHOD.  Without
  ;; walkers of their own the converter would expand DEFINE-METHOD and then
  ;; rewrite the DEFMETHOD inside it into DEFINE-METH, quietly turning a CLOS
  ;; method into a single-namespace definition.
  (eval-str "(define-class cls-d () () (iv))")
  (eval-str "(defparameter inst-d (make-instance cls-d))")
  (eval-str "(set-slot inst-d 'iv 21)")
  (is-equal 42 (eval-str "(progn (lisp1 (define-method dbl cls-d () (* (get-slot self 'iv) 2)))
                                 (dbl inst-d))")
            "a CLOS method defined inside LISP1 is still a CLOS method")
  (is-true (eval-str "(lisp1 (define-class cls-e () () (iv)))")
           "and DEFINE-CLASS survives conversion too"))

(deftest clos-classes-can-be-redefined (:phase "cl3" :severity :serious)
  ;; The DEFCLASS forms used to be the initial value of a DEFVAR, which does
  ;; not re-evaluate once the variable is bound.  Editing a class and
  ;; reloading therefore kept the old definition -- while the holder for the
  ;; class variables was reset unconditionally, so a redefinition was applied
  ;; exactly backwards: the part that should persist was wiped, and the part
  ;; that should change was not.
  (eval-str "(define-class red () (cv) (iv1))")
  (eval-str "(defparameter red-1 (make-instance red))")
  (eval-str "(set-slot red-1 'iv1 1)")
  (eval-str "(set-slot red 'cv 42)")
  ;; redefine, adding an instance variable
  (eval-str "(define-class red () (cv) (iv1 iv2))")
  (is-equal 9 (eval-str "(progn (defparameter red-2 (make-instance red))
                                (set-slot red-2 'iv2 9)
                                (get-slot red-2 'iv2))")
            "a slot added by redefinition exists")
  (is-equal 42 (eval-str "(get-slot red 'cv)")
            "and the class variables survive the redefinition")
  ;; redefining a superclass reaches the subclass too
  (eval-str "(progn (define-class res () (scv) (siv))
                    (define-class resub (res) () ()))")
  (eval-str "(define-class res () (scv scv2) (siv))")
  (is-equal 7 (eval-str "(progn (set-slot resub 'scv2 7) (get-slot resub 'scv2))")
            "a class variable added to a superclass reaches the subclass"))

(deftest clos-allows-a-slot-named-class (:phase "cl3" :severity :minor)
  ;; DEFINE-CLASS injected a slot called CLASS into every class it made.
  ;; Nothing ever read it -- the class variables are found through
  ;; *CLASS-INSTANCES* -- and it collided with any user slot of that name.
  (is-true (eval-str "(define-class recl () () (class other))")
           "a class may have a slot called CLASS")
  (is-equal 5 (eval-str "(progn (defparameter recl-1 (make-instance recl))
                                (set-slot recl-1 'class 5)
                                (get-slot recl-1 'class))")
            "and it is the user's slot, holding the user's value"))

(deftest clos-reports-a-class-it-does-not-know (:phase "cl3" :severity :minor)
  ;; Asking for the class variables of a class DEFINE-CLASS never made used to
  ;; reach SLOT-VALUE on NIL.
  (eval-str "(defclass reo () ((s :initform 1)))")
  (let ((message (handler-case (progn (eval-str "(get-slot (find-class 'reo) 's)") "")
                   (error (c) (princ-to-string c)))))
    (is-true (search "DEFINE-CLASS" message :test #'char-equal)
             "the error names DEFINE-CLASS rather than failing obscurely")))

(deftest single-namespace-does-not-need-the-rest (:phase "cl3" :severity :serious)
  ;; The three changes Common Lisp 3 makes are meant to be separable: loading
  ;; package.lisp and lisp1.lisp alone gives the single namespace with symbols
  ;; still case-insensitive and no layer over CLOS.  README.md documents that,
  ;; so nothing in lisp1.lisp may reach into clos-utils.lisp.
  (let* ((here (make-pathname :name nil :type nil :version nil
                              :defaults (or *load-truename*
                                            *default-pathname-defaults*)))
         (source (merge-pathnames "lisp1.lisp"
                                  (make-pathname
                                   :directory (butlast (pathname-directory here))
                                   :defaults here)))
         (text (with-open-file (in source)
                 (let ((s (make-string (file-length in))))
                   (subseq s 0 (read-sequence s in))))))
    (is-true (plusp (length text)) "lisp1.lisp is readable")
    ;; Call syntax, not bare mentions: DEFINE-CLASS and DEFINE-METHOD appear in
    ;; the walker table, where they are hash keys rather than dependencies --
    ;; registering a walker for a symbol does not require the macro to exist.
    (dolist (name '("(get-slot" "(set-slot" "(define-class" "(get-class-object"
                    "*class-instances*" "(parallel-class-name"))
      (is-equal nil (search name text :test #'char-equal)
                (format nil "lisp1.lisp never calls ~a)" (subseq name 1))))))

(deftest class-instance-variables-are-per-class (:phase "cl3" :severity :serious)
  ;; Smalltalk's other kind of class-side state: declared once, but every
  ;; class in the hierarchy has its own value.  The optional fifth argument
  ;; to DEFINE-CLASS, so a four-argument call means what it always did.
  (eval-str "(progn (define-class civ-a () (shared) (iv) (own))
                    (define-class civ-b (civ-a) () () ())
                    (define-class civ-c (civ-b) () () ()))")
  (is-equal '(1 2 3)
            (eval-str "(progn (set-slot civ-a 'own 1) (set-slot civ-b 'own 2)
                              (set-slot civ-c 'own 3)
                              (list (get-slot civ-a 'own) (get-slot civ-b 'own)
                                    (get-slot civ-c 'own)))")
            "every class keeps its own value")
  (is-equal 1 (eval-str "(get-slot civ-a 'own)")
            "and a subclass writing its own leaves the parent alone")
  (signals-error (eval-str "(progn (define-class civ-d () () () (o2))
                                   (define-class civ-e (civ-d) () () ())
                                   (set-slot civ-d 'o2 9)
                                   (get-slot civ-e 'o2))")
                 "unset in a subclass, it is unbound there")
  ;; The two kinds side by side, which is the point of having both.
  (is-equal '((5 5) (1 2))
            (eval-str "(progn (define-class civ-m () (sh) () (pc))
                              (define-class civ-n (civ-m) () () ())
                              (set-slot civ-n 'sh 5)
                              (set-slot civ-m 'pc 1) (set-slot civ-n 'pc 2)
                              (list (list (get-slot civ-m 'sh) (get-slot civ-n 'sh))
                                    (list (get-slot civ-m 'pc) (get-slot civ-n 'pc))))")
            "shared and per-class in one class, each behaving as declared")
  (is-equal '((0 0) (7 0))
            (eval-str "(progn (define-class civ-p () () () ((tally :initform 0)))
                              (define-class civ-q (civ-p) () () ())
                              (let ((before (list (get-slot civ-p 'tally) (get-slot civ-q 'tally))))
                                (set-slot civ-p 'tally 7)
                                (list before (list (get-slot civ-p 'tally) (get-slot civ-q 'tally)))))")
            ":initform starts each class off with its own copy")
  (is-equal '(1 2)
            (eval-str "(progn (define-class civ-r () () () (r)) (define-class civ-s (civ-r) () () ())
                              (set-slot civ-r 'r 1) (set-slot civ-s 'r 2)
                              (list (get-slot (make-instance civ-r) 'r)
                                    (get-slot (make-instance civ-s) 'r)))")
            "an instance reaches its own class's copy")
  (is-equal '(2 1)
            (eval-str "(progn (define-class civ-t1 () () () ((n :initform 0)))
                              (define-class civ-t2 (civ-t1) () () ())
                              (define-method tick civ-t1 () (set-slot self 'n (+ 1 (get-slot self 'n))))
                              (let ((a (make-instance civ-t1)) (b (make-instance civ-t1))
                                    (c (make-instance civ-t2)))
                                (tick a) (tick b) (tick c)
                                (list (get-slot civ-t1 'n) (get-slot civ-t2 'n))))")
            "a method keeps per-class state through SELF")
  ;; Redefinition: the shared kind needed a workaround on ECL and ABCL, so
  ;; check the per-class kind survives too.
  (is-equal '(3 4)
            (eval-str "(progn (define-class civ-u () (s) () (o))
                              (set-slot civ-u 's 3) (set-slot civ-u 'o 4)
                              (define-class civ-u () (s) (extra) (o))
                              (list (get-slot civ-u 's) (get-slot civ-u 'o)))")
            "both kinds survive a redefinition"))
