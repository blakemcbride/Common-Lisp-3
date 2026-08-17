

;  Common Lisp 3 -- the single namespace
;
;  Collapses the function and variable namespaces into one, so that a
;  function is an ordinary value: assignable, passable, and callable from
;  wherever it is bound.
;
;  Written by:
;  	Blake McBride
;  	blake@mcbridemail.com
;
;  Loaded by cl3.lisp, which owns the CL3 package.  See README.md for the
;  description and CHANGELOG.md for what changed.
;  
;  
;  
;  
;  
; Forms whose subforms are not all expressions -- binding lists, clause keys,
; declarations, type specifiers -- each need the converter to know their shape.
; They are registered in *FORM-WALKERS*; see DEFINE-FORM-WALKER, which is also
; how you teach the converter about a macro of your own.  Anything not
; registered is macroexpanded and its expansion walked.


(in-package :cl3)

(declaim (ftype function is-builtin convert convert-call convert-global-call
                convert-each convert-progn is-macro symbol-accessible-p
                rep quit-lisp check-call-arity
                expand-define expand-define-dynamic expand-define-macro
                expand-define-meth
                walk-lambda-list walk-specialized-lambda-list env-bind env-kind
                unconverted-warning))


;  ----------------------------------------------------------------------
;  Converter state
;
;  Declared here rather than beside the converter because the macros below
;  bind *CL-ENVIRONMENT*, and a special variable must be proclaimed before
;  the forms that bind it are compiled -- otherwise the binding is lexical
;  and the converter never sees it.
;  ----------------------------------------------------------------------

(defvar *cl-environment* nil
  "The &ENVIRONMENT of the macro currently being expanded.

Not optional.  CCL registers a compile-time macro definition in the
compilation environment only, so MACRO-FUNCTION without an environment
argument reports NIL there during COMPILE-FILE.  Without this, a macro
defined by DEFINE-MACRO and then used inside a converted body would be
compiled into a call to a non-existent variable.")

(defvar *expand-unknown-macros* t
  "When true, a macro with no registered walker is macroexpanded and its
expansion walked -- correct by construction, since the expansion is ordinary
Common Lisp whose expression positions the walker understands.  Set to NIL to
leave such forms alone instead; conversion then does not reach inside them.")

(defvar *warn-on-unconverted* t
  "When true, report any form Lisp1 declines to convert.  Silence is the wrong
default here: a form left alone is a place where Lisp1 semantics quietly do
not apply.")

(define-condition unconverted-form (style-warning)
  ((form   :initarg :form   :reader unconverted-form-form)
   (reason :initarg :reason :reader unconverted-form-reason))
  (:report (lambda (c s)
             (format s "Lisp1 did not convert ~s: ~a"
                     (unconverted-form-form c) (unconverted-form-reason c)))))


;  ----------------------------------------------------------------------
;  Global lexicals
;
;  DEFINE used to expand to DEFPARAMETER, which proclaims a name special
;  globally and permanently.  Every later LET or lambda parameter of that
;  name then became a dynamic binding, so closures captured nothing:
;
;      (define x 10)
;      (define f (lambda (x) (lambda () x)))
;      ((f 1))                         ; => 10, not 1
;
;  A hidden special cell plus DEFINE-SYMBOL-MACRO gives a global that a LET
;  can shadow lexically, which is what Lisp1 semantics require.  A symbol can
;  carry a symbol macro (variable namespace) and a macro (function namespace)
;  at the same time, so call dispatch is unaffected.
;  ----------------------------------------------------------------------

(defun hidden-symbol (prefix name)
  "A symbol private to Lisp1, derived from NAME and keyed by its home package
so that same-named symbols from different packages cannot collide."
  (let ((pkg (symbol-package name)))
    (intern (concatenate 'string prefix
                         (if pkg (package-name pkg) "#")
                         "::" (symbol-name name))
            "CL3-CELLS")))

(defun cell-symbol (name)
  "The cell holding NAME's global lexical value."
  (hidden-symbol "" name))

(defun gf-symbol (name)
  "The generic function standing behind NAME.  Methods cannot live on NAME
itself, because its function cell holds the call-dispatch macro; the space in
the prefix keeps these names clear of the value cells."
  (hidden-symbol "GENERIC " name))

(defun lisp1-error (control &rest args)
  (error (make-condition 'simple-error
                         :format-control control
                         :format-arguments args)))

(defun globally-special-p (name)
  "True when NAME is proclaimed globally special.  ANSI offers no predicate for
this, so probe it: a LET binding of a special name is visible to SYMBOL-VALUE,
a LET binding of a lexical name is not."
  (and (symbolp name)
       (not (constantp name))
       (let ((probe (list :probe)))
         (ignore-errors
           (eval `(let ((,name ',probe))
                    (declare (ignorable ,name))
                    (eq (symbol-value ',name) ',probe)))))
       t))

(defun global-lexical-p (name)
  "True when NAME is already a Lisp1 global lexical."
  (and (symbolp name)
       (multiple-value-bind (expansion expanded) (macroexpand-1 name)
         (and expanded
              (symbolp expansion)
              (eq (symbol-package expansion) (find-package "CL3-CELLS"))
              t))))

(defun check-definable (name operator)
  "Signal a clear error when NAME cannot become a global lexical."
  (unless (and name (symbolp name))
    (lisp1-error "~a: ~s is not a symbol." operator name))
  (when (constantp name)
    (lisp1-error "~a: ~s names a constant and cannot be redefined."
                 operator name))
  (when (globally-special-p name)
    (lisp1-error
     "~a: ~s is already a special variable, so it cannot become a global~%~
      lexical -- a symbol macro and a special proclamation cannot coexist.~%~
      The name was probably defined by DEFVAR or DEFPARAMETER, or by a~%~
      version of Lisp1 before 0.2, in which DEFINE expanded to DEFPARAMETER.~%~
      Start from a fresh image, or use DEFINE-DYNAMIC if a dynamically~%~
      scoped global is what you actually want."
     operator name))
  name)

(define-condition arity-mismatch (style-warning simple-condition) ()
  (:documentation "A defined name called with the wrong number of arguments.

A warning and not an error: the name holds a variable, and nothing stops it
being reassigned at run time to a function of a different arity."))

(defvar *check-arity* t
  "When true, a call to a name defined by DEFINE with a literal lambda is
checked for argument count at macroexpansion time.  This recovers some of what
the single namespace costs: a Lisp1 call compiles to FUNCALL of a variable,
which the compiler cannot check.")

(defun arity-description (min max)
  (cond ((and max (= min max)) (format nil "exactly ~d" min))
        ((null max)            (format nil "at least ~d" min))
        (t                     (format nil "between ~d and ~d" min max))))

(defun check-call-arity (name count min max)
  "Warn when NAME is called with COUNT arguments but takes MIN to MAX."
  (when (and *check-arity*
             (or (< count min) (and max (> count max))))
    (warn 'arity-mismatch
          :format-control "~s called with ~d argument~:p, but takes ~a."
          :format-arguments (list name count (arity-description min max))))
  nil)

(defun lambda-list-arity (lambda-list)
  "The number of arguments LAMBDA-LIST accepts, as MIN and MAX; MAX is NIL
when unbounded."
  (let ((min 0) (max 0) (optional nil))
    (dolist (item lambda-list (values min max))
      (cond ((and item (symbolp item) (member item lambda-list-keywords))
             (case item
               (&optional (setq optional t))
               (&aux      (return (values min max)))
               (t         (return (values min nil)))))   ; &rest, &body, &key
            (optional (incf max))
            (t (incf min) (incf max))))))

(defun literal-lambda-arity (form)
  "MIN and MAX for FORM when it is written as a literal lambda, else NIL."
  (when (and (consp form) (eq (car form) 'lambda) (listp (cadr form)))
    (multiple-value-bind (min max) (lambda-list-arity (cadr form))
      (cons min max))))

(defun dispatch-macro-form (name rest-var arity)
  "The macro that turns (NAME arg...) into a call of NAME's value.

The arity check is guarded by FBOUNDP rather than called outright, because a
compiled file can be loaded into an image where the LISP1 package exists but
the library itself does not -- which examples.lisp documents and relies on.
Checking then simply does not happen; the call still works."
  `(defmacro ,name (&rest ,rest-var)
     ,@(when arity
         `((when (fboundp 'check-call-arity)
             (funcall 'check-call-arity
                      ',name (length ,rest-var) ,(car arity) ,(cdr arity)))))
     (list* 'funcall ',name ,rest-var)))

(defun global-lexical-forms (name value-form &key (dispatch t) arity)
  "Forms binding NAME as a global lexical holding VALUE-FORM, and making
(NAME arg...) call that value.

DISPATCH nil omits the call-dispatch macro, for callers adding to a name that
already has one.  DEFMETHOD is the case that matters: several methods on one
name are ordinary, and re-defining the same macro once per method is a
duplicate definition -- which CCL reports and counts as a compilation
failure."
  (let ((cell (cell-symbol name))
        (rest (gensym "ARGS")))
    (append (list `(defvar ,cell)
                  `(setf ,cell ,value-form)
                  `(define-symbol-macro ,name ,cell))
            (when dispatch
              (list (dispatch-macro-form name rest arity))))))


;  ----------------------------------------------------------------------
;  Reaching a definition from ordinary Common Lisp
;
;  A symbol cannot hold both a macro and a function, and NAME's function cell
;  holds the call-dispatch macro, so #'NAME and (trace NAME) cannot work.  The
;  value is reachable though -- NAME is a symbol macro for its cell, so plain
;  CL code can write (funcall name 1 2) or (mapcar name list).  What follows
;  covers the rest.
;  ----------------------------------------------------------------------

(defun value-of (name)
  "The value NAME currently holds."
  (unless (global-lexical-p name)
    (lisp1-error "VALUE-OF: ~s was not defined by Lisp1." name))
  (let ((cell (cell-symbol name)))
    (unless (boundp cell)
      (lisp1-error "VALUE-OF: ~s has no value." name))
    (symbol-value cell)))

(defun (setf value-of) (new name)
  (unless (global-lexical-p name)
    (lisp1-error "(SETF VALUE-OF): ~s was not defined by Lisp1." name))
  (setf (symbol-value (cell-symbol name)) new))

(defun function-of (name)
  "The function NAME holds -- what #'NAME would give if NAME were a function.
Use it where Common Lisp wants a function object and the name is Lisp1's."
  (let ((value (value-of name)))
    (unless (functionp value)
      (lisp1-error "FUNCTION-OF: ~s holds ~s, which is not a function."
                   name value))
    value))

(defvar *traced* (make-hash-table :test 'eq)
  "Traced name -> the function it held before tracing.")

(defun trace-calls (name)
  "Trace calls to NAME.  CL:TRACE works through the function cell, which here
holds the dispatch macro, so it cannot be used.

Not named TRACE-FUNCTION: CCL's CL-USER already inherits CCL:TRACE-FUNCTION,
and (use-package \"LISP1\") there would signal a name conflict.  Avoiding a
clash with COMMON-LISP is not enough -- what matters is every symbol already
accessible in the package the user will use LISP1 from."
  (unless (gethash name *traced*)
    (let ((original (function-of name)))
      (setf (gethash name *traced*) original)
      (setf (value-of name)
            (lambda (&rest args)
              (format *trace-output* "~&  ~s called with ~s~%" name args)
              (let ((values (multiple-value-list (apply original args))))
                (format *trace-output* "~&  ~s returned ~{~s~^, ~}~%" name values)
                (values-list values))))))
  name)

(defun untrace-calls (name)
  "Stop tracing NAME."
  (let ((original (gethash name *traced*)))
    (when original
      (setf (value-of name) original)
      (remhash name *traced*)))
  name)

(defmacro lisp1 (&rest argsz &environment env)
  (let ((*cl-environment* env))
    (cons 'progn (convert-each argsz nil))))

(defun expand-define (name value env)
  "The body of DEFINE, taking the lexical environment explicitly.

Both DEFINE itself and the converter's walker for it come through here.  The
walker matters: a definition written inside a binding form is converted with
that binding in scope, where before it was converted with none, so

    (lisp1 (let ((list (lambda (n) (* n 2))))
             (define f (lambda (x) (list x)))))

called CL:LIST rather than the local binding -- finding 5 again, one level in."
  (check-definable name 'define)
  `(progn
     ,@(global-lexical-forms name (convert value env)
                             :arity (literal-lambda-arity value))
     ',name))

(defmacro define (name value &environment env)
  "Define NAME as a global lexical holding VALUE, callable as (NAME arg...).
Unlike DEFPARAMETER, the name is not proclaimed special, so a LET or lambda
parameter of the same name shadows it lexically and closures behave."
  (let ((*cl-environment* env))
    (expand-define name value nil)))

(defun expand-define-dynamic (name value env)
  (unless (and name (symbolp name))
    (lisp1-error "DEFINE-DYNAMIC: ~s is not a symbol." name))
  (when (global-lexical-p name)
    (lisp1-error "DEFINE-DYNAMIC: ~s is already a global lexical; a special~%~
                  proclamation cannot be made for a symbol macro." name))
  (let ((rest (gensym "ARGS")))
    `(progn
       (defparameter ,name ,(convert value env))
       ,(dispatch-macro-form name rest (literal-lambda-arity value))
       ',name)))

(defmacro define-dynamic (name value &environment env)
  "Define NAME as a dynamically scoped global -- what DEFINE did before 0.2.
For the occasions when dynamic scope is genuinely wanted; DEFINE is lexical."
  (let ((*cl-environment* env))
    (expand-define-dynamic name value nil)))

(defun expand-define-macro (name args body env)
  (multiple-value-bind (lambda-list body-env) (walk-lambda-list args env t)
    (list* 'defmacro name lambda-list (convert-each body body-env))))

(defmacro define-macro (name args &rest rest &environment env)
  "Define a macro whose body is Lisp1 source.  The macro lambda list is a
destructuring one, and its initialisation forms are converted too."
  (let ((*cl-environment* env))
    (expand-define-macro name args rest nil)))

(defun expand-define-meth (name rest env)
  "The body of DEFINE-METH, taking the lexical environment explicitly.

Qualifiers are parsed rather than assumed away, so :before, :after and :around
methods work.  They are the non-list forms between the name and the lambda
list; NIL is a list, so an empty lambda list ends the run correctly."
  (check-definable name 'define-meth)
  (let ((qualifiers '()))
    (loop while (and rest (not (listp (car rest))))
          do (push (pop rest) qualifiers))
    (let* ((qualifiers (nreverse qualifiers))
           (lambda-list (pop rest))
           (gf (gf-symbol name))
           ;  A top level DEFMACRO is available at compile time, so on the
           ;  second and later methods for a name this is already true and the
           ;  dispatch macro is left alone.  The environment argument is not
           ;  optional: CCL registers a compile-time macro in the compilation
           ;  environment only, so MACRO-FUNCTION without it reports NIL there
           ;  and every method would re-emit the macro.
           (dispatch (not (macro-function name *cl-environment*))))
      (multiple-value-bind (converted-list body-env)
          (walk-specialized-lambda-list lambda-list env)
        ;  CLOS makes these available inside a method body, but they are not
        ;  globally fbound, so without binding them here (call-next-method)
        ;  converts to a reference to a variable that does not exist.
        (setq body-env
              (env-bind :fun '(call-next-method next-method-p) body-env))
        `(progn
           (defmethod ,gf ,@qualifiers ,converted-list
             ,@(convert-each rest body-env))
           ,@(global-lexical-forms name `(function ,gf) :dispatch dispatch)
           ',name)))))

(defmacro define-meth (name &rest rest &environment env)
  "Add a method to the generic function held in NAME.

The methods live on a hidden generic function rather than on NAME itself:
NAME's function cell holds the call-dispatch macro, and DEFMETHOD on a symbol
that already names a macro is an error -- which previously limited a name to
one method, and destroyed that method when a second was attempted."
  (let ((*cl-environment* env))
    (expand-define-meth name rest nil)))


;  ----------------------------------------------------------------------
;  Interactive
;  ----------------------------------------------------------------------

(defparameter *prompt* "lisp1> ")

(defvar *rep-debugger* nil
  "When true, let a condition inside REP reach the host debugger.  The default
reports it and carries on with the next form, which is what a loop is for: the
old REP unwound out of itself on the first error, and reaching end of input --
Ctrl-D, the ordinary way to leave a REPL -- dropped the user into the debugger
rather than exiting.")

(defun quit-lisp (&optional (code 0))
  "Terminate the Lisp image with exit status CODE.

Tested on SBCL, CCL, ECL, ABCL and CLISP; the remaining branches follow each
implementation's published idiom.  REPQ used to have branches for SBCL and
CLISP alone, so on ECL, ABCL and CCL -- three of the four implementations the
README claims support for -- it returned without exiting.  SBCL's SB-EXT:QUIT
has long been deprecated in favour of SB-EXT:EXIT."
  (finish-output *standard-output*)
  (finish-output *error-output*)
  #+sbcl      (sb-ext:exit :code code)
  #+ccl       (ccl:quit code)
  #+ecl       (ext:quit code)
  #+clisp     (ext:quit code)
  #+abcl      (ext:quit :status code)
  #+cmu       (unix:unix-exit code)
  #+allegro   (excl:exit code :quiet t)
  #+lispworks (lispworks:quit :status code)
  #-(or sbcl ccl ecl clisp abcl cmu allegro lispworks)
  (lisp1-error "QUIT-LISP: no exit is known for ~a.  Add a branch here, or~%~
                call that implementation's own exit function."
               (lisp-implementation-type)))

(defun quit-form-p (form)
  "True for QUIT or EXIT, bare or as the head of a list, in any package.
Only the list forms were recognised before, so typing a bare QUIT evaluated it
as a variable and raised an unbound-variable error instead of leaving."
  (flet ((quit-name-p (x)
           ;  SYMBOLP first: SYMBOL-NAME on a number was a type error, which is
           ;  what made an input of (1 2) break the loop.
           (and x (symbolp x)
                (member (symbol-name x) '("QUIT" "EXIT") :test #'string=)
                t)))
    (or (quit-name-p form)
        (and (consp form) (quit-name-p (car form))))))

(defun shift-history (form values)
  "Maintain the standard REPL history variables."
  (setq +++ ++  ++ +  + form)
  (setq /// //  // /  / values)
  (setq *** **  ** *  * (first values)))

(defun rep (&key (input *standard-input*) (output *standard-output*))
  "A Lisp1 read-eval-print loop: each form is converted before evaluation, so
Lisp1 semantics apply to what you type, including bare top level expressions
over local function values.  Returns T when the user leaves."
  (let ((eof (list :eof))
        (failed (list :failed)))
    ;  Every branch below ends its output with a newline, so the next prompt
    ;  starts at the left margin.  Nothing is printed *before* a result: the
    ;  newline that ends the user's input has already moved the cursor, and a
    ;  FRESH-LINE here cannot know that -- it would add a blank line to every
    ;  interaction, since the terminal echoes input the output stream never
    ;  sees.
    (flet ((report (what condition)
             (format output "; ~a ~a: ~a~%" what (type-of condition) condition)
             (finish-output output)))
      (loop
        (princ *prompt* output)
        (finish-output output)
        (let ((form (if *rep-debugger*
                        (read input nil eof)
                        (handler-case (read input nil eof)
                          (error (c) (report "read error" c) failed)))))
          (cond
            ((eq form eof) (terpri output) (return t))
            ((eq form failed))                  ; reported; go round again
            ((quit-form-p form) (return t))
            (t
             (setq - form)
             (let ((values (if *rep-debugger*
                               (multiple-value-list
                                (common-lisp:eval (convert form)))
                               (handler-case
                                   (multiple-value-list
                                    (common-lisp:eval (convert form)))
                                 (error (c) (report "error" c) failed)))))
               (unless (eq values failed)
                 (shift-history form values)
                 (cond (values
                        (loop for value in values
                              for first = t then nil
                              do (unless first (terpri output))
                                 (prin1 value output)))
                       (t (princ "; no value" output)))
                 (terpri output))))))))))

(defun repq (&optional (code 0))
  "Run REP, then terminate the image with exit status CODE."
  (rep)
  (quit-lisp code))

(defun eval-form (x)
  "Convert X from Lisp1 to Common Lisp and evaluate it.

Named EVAL-FORM rather than shadowing CL:EVAL.  The package used to shadow
LOAD, COMPILE-FILE and EVAL while exporting none of them, so the shadowing
achieved nothing -- and exporting them as they stood would have broken every
CL:LOAD call in any package that used LISP1."
  (common-lisp:eval (convert x)))

;  CL3:LOAD and CL3:COMPILE-FILE are gone.  Stock Common Lisp tooling
;  handles Lisp1 source, because DEFINE and its relatives install real macros:
;
;      (compile-file "my-program")     ; ordinary CL:COMPILE-FILE
;      (load "my-program")             ; ordinary CL:LOAD, or ASDF, or SLIME
;
;  What they did that CL:LOAD does not is convert every top level form, so an
;  unwrapped expression using a lexically bound function value worked at top
;  level.  Wrap such a form in LISP1, which is what it is for:
;
;      (lisp1 (let ((f (lambda (x) (* x 2)))) (print (f 3))))
;
;  They are not worth keeping for that.  Between them they held three of the
;  audit's findings: COMPILE-FILE wrote its intermediate file with PRINT, so a
;  *PRINT-LENGTH* in the user's init file silently truncated the code it was
;  about to compile, and *PRINT-BASE* 16 wrote 255 as FF; it hardcoded
;  ./tmp.lisp, colliding between concurrent compiles and erroring outright if
;  the user already had that file; it discarded FAILURE-P, so a failed compile
;  returned quietly with no fasl and no error; and LOAD leaked an IN-PACKAGE
;  into the caller permanently and stopped early, without a word, on any file
;  containing the symbol CL3::EOF.

;  ----------------------------------------------------------------------
;  The converter
;
;  CONVERT rewrites Lisp1 source into Common Lisp.  The rule is simple -- a
;  form whose head names a value rather than a function becomes a FUNCALL --
;  but applying it correctly means knowing the shape of every form walked:
;  which subforms are expressions, which are binding lists, and which are
;  neither.  Three things make that work.
;
;  A lexical environment, so that a local binding shadows a COMMON-LISP name
;  exactly as a single namespace requires.  Without it (let ((list ...))
;  (list 21)) quietly called CL:LIST.
;
;  A table of walkers, one for each form whose subforms are not all
;  expressions, extended through DEFINE-FORM-WALKER.
;
;  A default that never guesses.  An unrecognised macro is macroexpanded and
;  the expansion walked, which is correct by construction: the expansion is
;  ordinary Common Lisp whose expression positions the walker understands.
;  Anything that cannot be expanded is left alone with a style warning.  The
;  old default -- assume every argument of an unrecognised form is an
;  expression -- is what corrupted FLET, LABELS, DESTRUCTURING-BIND, method
;  qualifiers and DEFINE-MACRO.
;  ----------------------------------------------------------------------

(defvar *form-walkers* (make-hash-table :test 'eq)
  "Head symbol -> function of (form env) returning the converted form.")

(defmacro define-form-walker (names (form env) &body body)
  "Teach the converter the shape of a form.  NAMES is a symbol or a list of
symbols; BODY returns the converted form.  Use this for a macro of your own
whose arguments are not all expressions, rather than letting the converter
macroexpand it."
  `(dolist (name ',(if (listp names) names (list names)))
     (setf (gethash name *form-walkers*)
           (lambda (,form ,env)
             (declare (ignorable ,form ,env))
             ,@body))))

(defun unconverted-warning (form reason)
  (when *warn-on-unconverted*
    (warn 'unconverted-form :form form :reason reason))
  form)


;  ---- lexical environment -------------------------------------------
;
;  A list of (kind . symbol), innermost binding first.  KIND is :VAR for an
;  ordinary variable, :FUN for a FLET/LABELS function, :MACRO for a MACROLET
;  macro, and :SYMBOL-MACRO for a SYMBOL-MACROLET name.  Variables and
;  functions live in different Common Lisp namespaces, so both are tracked
;  and the innermost entry wins.

(defun env-bind (kind names env)
  (dolist (name names env)
    (when (and name (symbolp name) (not (member name lambda-list-keywords)))
      (push (cons kind name) env))))

(defun env-kind (name env)
  (let ((entry (find name env :key #'cdr :test #'eq)))
    (and entry (car entry))))


;  ---- walking a list of forms ---------------------------------------

(defun convert-each (forms env)
  "Convert every form in FORMS, which may be an improper list."
  (cond ((null forms) nil)
        ((atom forms) forms)
        (t (cons (convert (car forms) env)
                 (convert-each (cdr forms) env)))))

(defun convert-progn (forms &optional env)
  "Retained under its original name; CONVERT-EACH is the same thing."
  (convert-each forms env))

(defun split-improper (list)
  "Return the proper prefix of LIST and its dotted tail, if any."
  (let ((items '()) (tail list))
    (loop while (consp tail) do (push (pop tail) items))
    (values (nreverse items) tail)))


;  ---- lambda lists ---------------------------------------------------

(defun walk-lambda-list (lambda-list env &optional destructuring)
  "Convert the initialisation forms in LAMBDA-LIST and collect its variables.
Returns the converted lambda list and an environment including its variables.

An &optional or &key initialisation form is converted in the environment of
the parameters preceding it, which is where it is evaluated.  DESTRUCTURING
allows nested patterns and a dotted tail, for DEFMACRO and
DESTRUCTURING-BIND."
  (multiple-value-bind (items tail) (split-improper lambda-list)
    (let ((out '()) (state :required))
      (flet ((bind (name)
               (when (and name (symbolp name) (not (constantp name)))
                 (push (cons :var name) env))))
        (dolist (item items)
          (cond
            ((and (symbolp item) (member item lambda-list-keywords))
             (setq state item)
             (push item out))
            ((member state '(&optional &key &aux))
             (cond
               ((symbolp item) (bind item) (push item out))
               ((consp item)
                ;  (var init supplied-p), or ((:keyword var) init supplied-p)
                (let* ((var (first item))
                       (has-init (cdr item))
                       (init (and has-init (convert (second item) env)))
                       (supplied (third item)))
                  (if (consp var) (bind (second var)) (bind var))
                  (bind supplied)
                  (push (cond ((cddr item) (list var init supplied))
                              (has-init    (list var init))
                              (t           item))
                        out)))
               (t (push item out))))
            (t
             ;  required, &rest, &body, &whole, &environment
             (cond
               ((symbolp item) (bind item) (push item out))
               ((and destructuring (consp item))
                (multiple-value-bind (sub sub-env) (walk-lambda-list item env t)
                  (setq env sub-env)
                  (push sub out)))
               (t (push item out))))))
        (when (and tail (symbolp tail)) (bind tail)))
      (values (if tail (append (nreverse out) tail) (nreverse out))
              env))))

(defun walk-specialized-lambda-list (lambda-list env)
  "As WALK-LAMBDA-LIST, for a method lambda list.  A specializer is a type
name, not an expression, and is left alone -- except the form inside an EQL
specializer, which is evaluated and so is converted."
  (let ((required '()) (rest lambda-list))
    (loop while (and (consp rest) (not (member (car rest) lambda-list-keywords)))
          do (push (pop rest) required))
    (let ((out '()))
      (dolist (param (nreverse required))
        (cond
          ((symbolp param)
           (setq env (env-bind :var (list param) env))
           (push param out))
          ((consp param)
           (setq env (env-bind :var (list (first param)) env))
           (let ((specializer (second param)))
             (push (if (and (consp specializer) (eq (car specializer) 'eql))
                       (list (first param)
                             (list 'eql (convert (second specializer) env)))
                       param)
                   out)))
          (t (push param out))))
      (multiple-value-bind (converted-rest new-env) (walk-lambda-list rest env)
        (values (append (nreverse out) converted-rest) new-env)))))

(defun walk-local-function (definition env &optional destructuring)
  "Convert one FLET / LABELS / MACROLET definition."
  (multiple-value-bind (lambda-list body-env)
      (walk-lambda-list (second definition) env destructuring)
    (list* (first definition) lambda-list
           (convert-each (cddr definition) body-env))))


;  ---- forms left exactly as written ----------------------------------
;
;  Their subforms are declarations, type specifiers, package designators or
;  slot descriptions -- none of them expressions.

(define-form-walker (quote go declare declaim
                     defpackage in-package defclass defstruct deftype
                     defgeneric define-condition define-setf-expander defsetf
                     define-compiler-macro loop-finish
                     ;  The layer over CLOS.  These expand to DEFCLASS and
                     ;  DEFMETHOD and look after themselves; without a walker
                     ;  DEFINE-METHOD would be expanded, the DEFMETHOD inside
                     ;  it rewritten into DEFINE-METH, and a CLOS method
                     ;  quietly turned into a single-namespace definition.
                     define-class define-method)
    (form env)
  form)

;  Our own definers convert their own bodies; walking them here would convert
;  twice, and macroexpanding them would convert the expansion a second time.
;  This is what used to rewrite (define-macro m (a b) ...) into
;  (define-macro m (funcall a b) ...).

(define-form-walker define (form env)
  (expand-define (second form) (third form) env))

(define-form-walker define-dynamic (form env)
  (expand-define-dynamic (second form) (third form) env))

(define-form-walker define-macro (form env)
  (expand-define-macro (second form) (third form) (cdddr form) env))

(define-form-walker define-meth (form env)
  (expand-define-meth (second form) (cddr form) env))


;  ---- forms whose subforms are all expressions ------------------------

(define-form-walker (progn if catch throw unwind-protect progv
                     multiple-value-call multiple-value-prog1
                     when unless and or cond-less-and
                     prog1 prog2 return ignore-errors time
                     push pushnew pop incf decf
                     setf psetf shiftf rotatef nth-value multiple-value-list)
    (form env)
  (cons (car form) (convert-each (cdr form) env)))


;  ---- binding and clause forms ---------------------------------------

(define-form-walker lambda (form env)
  (multiple-value-bind (lambda-list body-env) (walk-lambda-list (second form) env)
    (list* 'lambda lambda-list (convert-each (cddr form) body-env))))

(define-form-walker function (form env)
  (let ((arg (second form)))
    (if (and (consp arg) (eq (car arg) 'lambda))
        (list 'function (convert arg env))
        form)))

(define-form-walker let (form env)
  (let ((bindings '()) (body-env env))
    (dolist (binding (second form))
      (cond ((consp binding)
             (push (if (cdr binding)
                       (list (first binding) (convert (second binding) env))
                       binding)
                   bindings)
             (setq body-env (env-bind :var (list (first binding)) body-env)))
            (t (push binding bindings)
               (setq body-env (env-bind :var (list binding) body-env)))))
    (list* 'let (nreverse bindings) (convert-each (cddr form) body-env))))

(define-form-walker let* (form env)
  (let ((bindings '()) (body-env env))
    (dolist (binding (second form))
      (cond ((consp binding)
             (push (if (cdr binding)
                       (list (first binding) (convert (second binding) body-env))
                       binding)
                   bindings)
             (setq body-env (env-bind :var (list (first binding)) body-env)))
            (t (push binding bindings)
               (setq body-env (env-bind :var (list binding) body-env)))))
    (list* 'let* (nreverse bindings) (convert-each (cddr form) body-env))))

(define-form-walker flet (form env)
  (let ((body-env (env-bind :fun (mapcar #'car (second form)) env)))
    (list* 'flet
           (mapcar (lambda (d) (walk-local-function d env)) (second form))
           (convert-each (cddr form) body-env))))

(define-form-walker labels (form env)
  ;  Unlike FLET, the definitions can see one another.
  (let ((body-env (env-bind :fun (mapcar #'car (second form)) env)))
    (list* 'labels
           (mapcar (lambda (d) (walk-local-function d body-env)) (second form))
           (convert-each (cddr form) body-env))))

(define-form-walker macrolet (form env)
  (let ((body-env (env-bind :macro (mapcar #'car (second form)) env)))
    (list* 'macrolet
           (mapcar (lambda (d) (walk-local-function d env t)) (second form))
           (convert-each (cddr form) body-env))))

(define-form-walker symbol-macrolet (form env)
  (let ((body-env (env-bind :symbol-macro (mapcar #'car (second form)) env)))
    (list* 'symbol-macrolet
           (mapcar (lambda (d) (list (first d) (convert (second d) env)))
                   (second form))
           (convert-each (cddr form) body-env))))

(define-form-walker (block return-from) (form env)
  ;  The block name is a name, not an expression.
  (list* (first form) (second form) (convert-each (cddr form) env)))

(define-form-walker tagbody (form env)
  ;  Atoms in the body are tags.
  (cons 'tagbody
        (mapcar (lambda (f) (if (atom f) f (convert f env))) (cdr form))))

(define-form-walker the (form env)
  (list 'the (second form) (convert (third form) env)))

(define-form-walker eval-when (form env)
  (list* 'eval-when (second form) (convert-each (cddr form) env)))

(define-form-walker locally (form env)
  (cons 'locally (convert-each (cdr form) env)))

(define-form-walker load-time-value (form env)
  (list* 'load-time-value (convert (second form) env) (cddr form)))

(define-form-walker setq (form env)
  ;  Alternating name and expression; only the expressions are converted.
  (let ((out '()) (rest (cdr form)))
    (loop while rest
          do (push (pop rest) out)
             (when rest (push (convert (pop rest) env) out)))
    (cons 'setq (nreverse out))))

(define-form-walker multiple-value-setq (form env)
  (list 'multiple-value-setq (second form) (convert (third form) env)))

(define-form-walker cond (form env)
  (cons 'cond (mapcar (lambda (clause) (convert-each clause env)) (cdr form))))

(define-form-walker (case ccase ecase typecase ctypecase etypecase) (form env)
  ;  Clause keys are objects or type specifiers, never expressions.  Walking
  ;  them is what made (case 1 (1 :one)) abort and (case 'a ((a b) :ab))
  ;  quietly select the wrong branch.
  (list* (first form) (convert (second form) env)
         (mapcar (lambda (clause)
                   (cons (first clause) (convert-each (cdr clause) env)))
                 (cddr form))))

(define-form-walker (do do*) (form env)
  (let* ((sequential (eq (first form) 'do*))
         (bindings (second form))
         (names (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
         (full-env (env-bind :var names env))
         (step-env env)
         (converted
           (mapcar
            (lambda (binding)
              (if (consp binding)
                  (let* ((var (first binding))
                         (has-init (cdr binding))
                         (init (and has-init
                                    (convert (second binding)
                                             (if sequential step-env env))))
                         ;  A step form sees every variable, including those
                         ;  bound after it.
                         (step (and (cddr binding)
                                    (convert (third binding) full-env))))
                    (when sequential
                      (setq step-env (env-bind :var (list var) step-env)))
                    (cond ((cddr binding) (list var init step))
                          (has-init       (list var init))
                          (t              binding)))
                  binding))
            bindings)))
    (list* (first form) converted
           (convert-each (third form) full-env)
           (convert-each (cdddr form) full-env))))

(define-form-walker (dolist dotimes) (form env)
  (let* ((spec (second form))
         (var (first spec))
         (sequence (convert (second spec) env))
         (has-result (cddr spec))
         (body-env (env-bind :var (list var) env))
         ;  The result form is evaluated with the variable still bound.
         (result (and has-result (convert (third spec) body-env))))
    (list* (first form)
           (if has-result (list var sequence result) (list var sequence))
           (convert-each (cddr form) body-env))))

(define-form-walker multiple-value-bind (form env)
  (list* 'multiple-value-bind (second form) (convert (third form) env)
         (convert-each (cdddr form) (env-bind :var (second form) env))))

(define-form-walker destructuring-bind (form env)
  (multiple-value-bind (lambda-list body-env)
      (walk-lambda-list (second form) env t)
    (list* 'destructuring-bind lambda-list (convert (third form) env)
           (convert-each (cdddr form) body-env))))

(define-form-walker (prog prog*) (form env)
  ;  A LET with a TAGBODY for a body.
  (let ((bindings '()) (body-env env)
        (sequential (eq (first form) 'prog*)))
    (dolist (binding (second form))
      (cond ((consp binding)
             (push (if (cdr binding)
                       (list (first binding)
                             (convert (second binding)
                                      (if sequential body-env env)))
                       binding)
                   bindings)
             (setq body-env (env-bind :var (list (first binding)) body-env)))
            (t (push binding bindings)
               (setq body-env (env-bind :var (list binding) body-env)))))
    (list* (first form) (nreverse bindings)
           (mapcar (lambda (f) (if (atom f) f (convert f body-env)))
                   (cddr form)))))

(define-form-walker handler-case (form env)
  (list* 'handler-case (convert (second form) env)
         (mapcar (lambda (clause)
                   ;  (typespec (var) . body); the type is not an expression.
                   (multiple-value-bind (lambda-list body-env)
                       (walk-lambda-list (second clause) env)
                     (list* (first clause) lambda-list
                            (convert-each (cddr clause) body-env))))
                 (cddr form))))

(define-form-walker (handler-bind restart-bind) (form env)
  (list* (first form)
         (mapcar (lambda (b) (list (first b) (convert (second b) env)))
                 (second form))
         (convert-each (cddr form) env)))

(define-form-walker defun (form env)
  ;  A bare DEFPARAMETER used to come out of here, with no dispatch macro, so
  ;  the name was callable only from inside another converted form.  Hand it
  ;  to DEFINE, which converts the lambda itself.
  (list 'define (second form)
        (list* 'lambda (third form) (cdddr form))))

(define-form-walker defmacro (form env)
  (multiple-value-bind (lambda-list body-env) (walk-lambda-list (third form) env t)
    (list* 'defmacro (second form) lambda-list
           (convert-each (cdddr form) body-env))))

(define-form-walker defmethod (form env)
  ;  DEFINE-METH parses qualifiers and converts the body.
  (cons 'define-meth (cdr form)))

(define-form-walker (defvar defparameter defconstant) (form env)
  (if (cddr form)
      (list* (first form) (second form) (convert (third form) env) (cdddr form))
      form))


;  ---- LOOP -------------------------------------------------------------
;
;  LOOP gets a walker of its own rather than being macroexpanded.  Its
;  expansion is large and full of implementation internals -- on SBCL it
;  reaches SB-KERNEL:UNALIGNED-DX-CONS, which is not a function, a macro or a
;  special operator as far as the walker can tell.
;
;  This does not parse the LOOP grammar.  It classifies each token by the
;  keyword that preceded it, which is all that is needed to tell an
;  expression from a variable, a destructuring pattern or a type specifier.

(defparameter *loop-keywords*
  '("NAMED" "WITH" "FOR" "AS" "AND" "="
    "FROM" "DOWNFROM" "UPFROM" "TO" "DOWNTO" "UPTO" "BELOW" "ABOVE" "BY"
    "IN" "ON" "THEN" "ACROSS" "BEING" "THE" "EACH" "OF" "USING"
    "HASH-KEY" "HASH-KEYS" "HASH-VALUE" "HASH-VALUES"
    "SYMBOL" "SYMBOLS" "PRESENT-SYMBOL" "PRESENT-SYMBOLS"
    "EXTERNAL-SYMBOL" "EXTERNAL-SYMBOLS"
    "DO" "DOING" "COLLECT" "COLLECTING" "APPEND" "APPENDING"
    "NCONC" "NCONCING" "COUNT" "COUNTING" "SUM" "SUMMING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "INTO"
    "IF" "WHEN" "UNLESS" "ELSE" "END" "IT"
    "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS" "REPEAT"
    "INITIALLY" "FINALLY" "RETURN" "OF-TYPE")
  "Compared by name: LOOP accepts its keywords from any package.")

(defun loop-keyword-p (token)
  (and token (symbolp token)
       (member (symbol-name token) *loop-keywords* :test #'string=)
       t))

(defun loop-pattern-vars (pattern)
  "Every symbol in a LOOP destructuring pattern, which may be dotted."
  (cond ((null pattern) nil)
        ((symbolp pattern) (list pattern))
        ((consp pattern) (append (loop-pattern-vars (car pattern))
                                 (loop-pattern-vars (cdr pattern))))
        (t nil)))

(define-form-walker loop (form env)
  (let ((out '())
        (pending :expr)     ; what a non-keyword token here means
        (binder nil))       ; whether AND would resume a variable clause
    (dolist (token (cdr form))
      (cond
        ((loop-keyword-p token)
         (let ((name (symbol-name token)))
           (cond ((member name '("WITH" "FOR" "AS") :test #'string=)
                  (setq pending :var binder t))
                 ((string= name "AND")     (setq pending (if binder :var :expr)))
                 ((string= name "INTO")    (setq pending :var))
                 ((string= name "NAMED")   (setq pending :opaque))
                 ((string= name "OF-TYPE") (setq pending :opaque))
                 ((string= name "USING")   (setq pending :using))
                 ;  BEING / THE / EACH are followed by another keyword
                 ((member name '("BEING" "THE" "EACH") :test #'string=)
                  (setq pending :opaque))
                 (t (setq pending :expr binder nil)))
           (push token out)))
        (t
         (case pending
           (:var
            (setq env (env-bind :var (loop-pattern-vars token) env))
            (push token out)
            ;  A type specifier may follow a variable with no OF-TYPE.
            (setq pending :opaque))
           (:using
            (setq env (env-bind :var (loop-pattern-vars (cdr token)) env))
            (push token out)
            (setq pending :expr))
           (:opaque
            (push token out)
            (setq pending :expr))
           (t (push (convert token env) out))))))
    (cons 'loop (nreverse out))))


;  ---- the walker itself ----------------------------------------------

(defun convert (x &optional env)
  (cond
    ((atom x) x)
    ((consp (car x))
     ;  A form in head position.  ((lambda ...) ...) is a call Common Lisp
     ;  accepts as written; anything else evaluates to the function.
     (if (eq (caar x) 'lambda)
         (cons (convert (car x) env) (convert-each (cdr x) env))
         (list* 'funcall (convert (car x) env) (convert-each (cdr x) env))))
    ((not (symbolp (car x)))
     ;  A number, string or character in head position is data, not a call.
     ;  Rewriting it is wrong, and (STRING x) on it used to signal a type
     ;  error that aborted the whole macroexpansion.
     x)
    (t (convert-call x env))))

(defun convert-call (x env)
  (let ((head (car x)))
    (case (env-kind head env)
      ;  A lexically bound name is a value, whatever it is called.  Checked
      ;  before everything else, so (let ((list ...)) (list 21)) means the
      ;  local binding and not CL:LIST.
      ((:var :symbol-macro)
       (list* 'funcall head (convert-each (cdr x) env)))
      ;  FLET and LABELS bind in the function namespace: a direct call.
      (:fun
       (cons head (convert-each (cdr x) env)))
      (:macro
       (unconverted-warning x "it is a MACROLET macro, whose shape is unknown here"))
      (t (convert-global-call x env)))))

(defun convert-global-call (x env)
  (let* ((head (car x))
         (walker (gethash head *form-walkers*)))
    (cond
      (walker (funcall walker x env))
      ((special-operator-p head)
       (unconverted-warning
        x "it is a special operator with no walker; add one with DEFINE-FORM-WALKER"))
      ((macro-function head *cl-environment*)
       (if *expand-unknown-macros*
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 x *cl-environment*)
             (if expanded
                 (convert expansion env)
                 (unconverted-warning x "its macroexpansion did not change it")))
           (unconverted-warning x "macroexpansion of unknown macros is disabled")))
      ;  A COMMON-LISP function: every argument is an expression.  Compared by
      ;  identity, so a symbol merely NAMED "SECOND" in a package that shadows
      ;  it is not mistaken for CL:SECOND.
      ((and (is-builtin head) (fboundp head))
       (cons head (convert-each (cdr x) env)))
      ;  A function whose name the user could not have written, because the
      ;  symbol is not accessible in the current package.  It came from a
      ;  macroexpansion, so it is a call and its arguments are expressions.
      ;  Without this, an implementation internal reached by expanding a macro
      ;  would be rewritten into a FUNCALL of an unbound variable.
      ((and (fboundp head) (not (symbol-accessible-p head)))
       (cons head (convert-each (cdr x) env)))
      ;  Anything else names a value: the Lisp1 call.
      (t (list* 'funcall (convert head env) (convert-each (cdr x) env))))))

(defun symbol-accessible-p (symbol)
  "True when SYMBOL could have been written by name in the current package."
  (eq symbol (find-symbol (symbol-name symbol) *package*)))

(defun is-builtin (x)
  (and (symbolp x)
       (multiple-value-bind (symbol status)
           (find-symbol (symbol-name x) "COMMON-LISP")
         (and (eq status :external) (eq symbol x)))))

(defun is-macro (x &optional (env *cl-environment*))
  (and (symbolp x) (macro-function x env) t))
