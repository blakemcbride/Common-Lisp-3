
;  Common Lisp CLOS utilities
;
;  Written by:
;        Blake McBride
;        blake@mcbridemail.com
;
;  A layer over CLOS giving classes their own class-side state, and a
;  shorter way to write a method.
;
;  Smalltalk has two kinds of class-side state, and so does this:
;
;    class variables           one value, shared by the class that declares
;                              it and every subclass.  Setting it through a
;                              subclass, or through an instance, sets the
;                              value the whole hierarchy sees.
;
;    class-instance variables  declared once, but every class in the
;                              hierarchy gets its own value -- a per-class
;                              counter or registry.
;
;  Neither is visible above the class that declares it.  Both are CLOS
;  shared slots of the class itself; see DEFINE-CLASS.
;
(in-package :cl3)

;  Class-side state lives in the class itself, as CLOS shared slots
;  (:allocation :class), so that an instance reaches it as it reaches any
;  slot -- an ordinary SLOT-VALUE, which the compiler optimises.  It used to
;  live in a parallel class, reached from an instance by asking SLOT-EXISTS-P
;  on every access, which made GET-SLOT eight times the cost of an accessor.
;
;  A class variable is a shared slot declared once, so CLOS gives the
;  declaring class and every subclass the one location.  A class-instance
;  variable is a shared slot declared afresh in every class below, which CLOS
;  gives a location of its own: DEFINE-CLASS redeclares the ones a class
;  inherits.  The class side is reached through an instance made by
;  ALLOCATE-INSTANCE, which shares those slots and runs no initialisation.

(defvar *class-instances* (make-hash-table :test #'eq)
  "Class object -> the uninitialised instance through which GET-SLOT and
SET-SLOT reach its class variables.")

(defvar *class-instance-variables* (make-hash-table :test #'eq)
  "Class name -> the class-instance variable specifications it has, its own
and those it inherits, which every subclass must declare again.")

(defun slot-spec-name (spec)
  (if (consp spec) (car spec) spec))

(defun inherited-class-instance-variables (supers own)
  "The class-instance variable specifications SUPERS have, nearest first,
less any of the names in OWN."
  (let ((result '()))
    (dolist (super supers (nreverse result))
      (dolist (spec (gethash super *class-instance-variables*))
        (let ((name (slot-spec-name spec)))
          (unless (or (member name own)
                      (find name result :key #'slot-spec-name))
            (push spec result)))))))

(defmacro define-class (name super-class-list class-variables instance-variables
                        &optional class-instance-variables)
  "Define a class with class variables, instance variables and, optionally,
class-instance variables.

CLASS-VARIABLES are shared with every subclass: one value for the hierarchy.
CLASS-INSTANCE-VARIABLES are declared once but give every class its own
value.  Both are read and written with GET-SLOT and SET-SLOT on the class or
on any instance.  An instance variable of the same name takes precedence.
The fifth argument is optional, so a four-argument call means what it always
did.

Re-evaluating this redefines the class, and the class-side values that
survive the change keep their values."
  (let* ((ivs (mapcar #'slot-spec-name instance-variables))
         (unshadowed (lambda (specs)
                       (remove-if (lambda (spec) (member (slot-spec-name spec) ivs))
                                  specs)))
         (own-civs (funcall unshadowed class-instance-variables))
         (all-civs (append own-civs
                           (funcall unshadowed
                                    (inherited-class-instance-variables
                                     super-class-list
                                     (append ivs
                                             (mapcar #'slot-spec-name own-civs)
                                             (mapcar #'slot-spec-name class-variables))))))
         (cvs (funcall unshadowed class-variables))
         (class-side (append (mapcar #'slot-spec-name cvs)
                             (mapcar #'slot-spec-name all-civs))))
    `(progn
       ;  Known as a variable while the rest of the file is compiled, not only
       ;  once it is loaded: DEFPARAMETER promises that on some implementations
       ;  but not others, and without it single-namespace code naming the class
       ;  takes the name for a forward reference to a DEFINE.
       (eval-when (:compile-toplevel)
         (proclaim '(special ,name)))
       ;  For subclasses, which may be defined in the same file.
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *class-instance-variables*) ',all-civs))
       ;  Captured before the DEFCLASS below, restored after it; see
       ;  *SAVED-CLASS-VARIABLES*.
       (setf (gethash ',name *saved-class-variables*)
             (class-variable-values (find-class ',name nil) ',class-side))
       (defclass ,name ,super-class-list
         (,@instance-variables
          ,@(mapcar #'shared-slot-spec cvs)
          ,@(mapcar #'shared-slot-spec all-civs)))
       (defparameter ,name (find-class ',name))
       (unless (gethash ,name *class-instances*)
         (setf (gethash ,name *class-instances*) (allocate-instance ,name)))
       (restore-class-variables ,name (gethash ',name *saved-class-variables*))
       (remhash ',name *saved-class-variables*)
       ,name)))

(defvar *saved-class-variables* (make-hash-table :test #'eq)
  "Class name -> values captured just before a redefinition.

CLHS 4.3.6 says the value of a slot shared in both the old and the new class
is retained when a class is redefined.  SBCL, CCL and CLISP do that; ECL and
ABCL lose it, in pure CLOS with none of this code involved.  DEFINE-CLASS
therefore saves the class-side values before redefining and puts them back
afterwards, so they survive a redefinition everywhere.")

(defun shared-slot-spec (spec)
  "SPEC as a shared slot.  An explicit :allocation is left alone."
  (let ((spec (if (consp spec) spec (list spec))))
    (if (member :allocation spec)
        spec
        (append spec '(:allocation :class)))))

(defun class-variable-values (class names)
  "An alist of the bound values among NAMES for CLASS, or NIL."
  (let ((holder (and class (gethash class *class-instances*))))
    (when holder
      (let ((values '()))
        (dolist (name names (nreverse values))
          (when (and (slot-exists-p holder name) (slot-boundp holder name))
            (push (cons name (slot-value holder name)) values)))))))

(defun restore-class-variables (class values)
  "Put VALUES back into CLASS's class variables."
  (let ((holder (and class (gethash class *class-instances*))))
    (when holder
      (dolist (pair values)
        (when (slot-exists-p holder (car pair))
          (setf (slot-value holder (car pair)) (cdr pair))))))
  class)

(defun get-class-object (cls)
  "The instance holding CLS's class variables."
  (or (gethash cls *class-instances*)
      (error "~s has no class variables: it was not defined by DEFINE-CLASS."
             cls)))

(defmacro class-object-p (object)
  "True when OBJECT is a class rather than an instance.  Every class
DEFINE-CLASS makes is a STANDARD-CLASS, and comparing CLASS-OF is several
times cheaper than (TYPEP OBJECT 'CLASS), which is most of what a slot
access cost on SBCL."
  `(eq (class-of ,object) (load-time-value (find-class 'standard-class))))

(defun get-slot (obj slot)
  "The value of SLOT in OBJ, which may be an instance or a class.

An instance reaches its own instance variables first and then its class
variables, so that a method can read either through SELF, as in Smalltalk.
An instance variable shadows a class variable of the same name.  Both are
slots of the instance, so this is SLOT-VALUE; a class is read through the
instance kept for it."
  (if (class-object-p obj)
      (slot-value (get-class-object obj) slot)
      (slot-value obj slot)))

(defun set-slot (obj slot val)
  "Set SLOT in OBJ to VAL.  Resolved as GET-SLOT resolves it, so setting a
class variable through an instance sets the one value the class shares."
  (if (class-object-p obj)
      (setf (slot-value (get-class-object obj) slot) val)
      (setf (slot-value obj slot) val)))

;  With the slot name a constant, as it nearly always is, an instance's slot is
;  a SLOT-VALUE the compiler can see, which SBCL makes as fast as an accessor.
;  Inside a DEFINE-METHOD body, SELF is known to be an instance, so for it even
;  the class-or-instance test goes: DEFINE-METHOD marks its body with the
;  symbol macro below, which the compiler macros look for in their
;  environment.  (Rebinding SELF to a class inside a method body defeats
;  this; nothing else does.)

(defun quoted-symbol-p (form)
  (and (consp form) (eq (car form) 'quote) (consp (cdr form))
       (symbolp (cadr form)) (null (cddr form))))

(defun method-self-p (form env)
  "True when FORM is SELF inside a DEFINE-METHOD body."
  (and (eq form 'self)
       (nth-value 1 (macroexpand-1 'in-define-method env))))

(define-compiler-macro get-slot (&whole form obj slot &environment env)
  (cond ((not (quoted-symbol-p slot)) form)
        ((method-self-p obj env) `(slot-value ,obj ,slot))
        (t
         (let ((o (gensym "OBJ")))
           `(let ((,o ,obj))
              (if (class-object-p ,o)
                  (slot-value (get-class-object ,o) ,slot)
                  (slot-value ,o ,slot)))))))

(define-compiler-macro set-slot (&whole form obj slot val &environment env)
  (cond ((not (quoted-symbol-p slot)) form)
        ((method-self-p obj env) `(setf (slot-value ,obj ,slot) ,val))
        (t
         (let ((o (gensym "OBJ")) (v (gensym "VAL")))
           `(let ((,o ,obj) (,v ,val))
              (if (class-object-p ,o)
                  (setf (slot-value (get-class-object ,o) ,slot) ,v)
                  (setf (slot-value ,o ,slot) ,v)))))))

;  ---- methods ---------------------------------------------------------
;
;  DEFINE-METHOD lets one name take a different number of arguments in
;  different classes, which a single CLOS generic function cannot: its methods
;  must agree on their lambda lists.  It used to give every method the lambda
;  list (SELF &REST ARGS) and APPLY its body to them, which cost more than the
;  rest of a method call together.
;
;  Each argument count now has a generic function of its own, hidden in
;  CL3-CELLS, with a fixed lambda list.  A call whose argument count is known
;  when it is compiled -- nearly every call -- goes straight to it through a
;  compiler macro.  NAME itself is a function choosing by argument count, for
;  FUNCALL, APPLY and use as a value.  A method with &OPTIONAL, &REST or &KEY
;  goes on one more, variadic, generic function, which a call reaches when no
;  fixed-count method applies.

(defvar *method-arities* (make-hash-table :test #'eq)
  "Name -> the argument counts DEFINE-METHOD has given it fixed methods for.
Recorded at compile time too, for the compiler macro.")

(defun method-gf-name (name arity)
  "The hidden generic function for NAME's methods taking ARITY arguments
after the instance, or NIL for its variadic methods.

Remembered on NAME's property list: the dispatching function asks on every
call made through it -- FUNCALL of a value, APPLY, MAPCAR -- and building
the name with FORMAT and INTERN each time made such a call thirty times the
cost of a direct one."
  (let* ((key (or arity :variadic))
         (known (assoc key (get name 'method-gf-names))))
    (if known
        (cdr known)
        (let ((symbol
                (intern (format nil "METHOD ~a::~a/~a"
                                (let ((p (symbol-package name)))
                                  (if p (package-name p) "#"))
                                (symbol-name name)
                                (or arity "*"))
                        "CL3-CELLS")))
          (push (cons key symbol) (get name 'method-gf-names))
          symbol))))

(defun note-method-arity (name arity)
  (when arity
    (pushnew arity (gethash name *method-arities*)))
  name)

(defun fixed-arg-list-p (arg-list)
  (notany (lambda (x) (member x lambda-list-keywords)) arg-list))

(defun foreign-generic-function-p (name)
  "True when NAME is a generic function DEFINE-METHOD did not make, such as
one DEFMETHOD made.  DEFINE-METHOD then adds an ordinary method to it."
  (and (fboundp name)
       (not (get name 'define-method))
       (typep (fdefinition name) 'generic-function)))

(defun no-method-error (name self args)
  (error "~s has no method for ~s taking ~d argument~:p after the instance."
         name self (length args)))

(defun call-variadic-method (name self args)
  "Call NAME's variadic method for SELF, if it has one."
  (let ((gf (method-gf-name name nil)))
    (if (and (fboundp gf)
             (compute-applicable-methods (fdefinition gf) (cons self args)))
        (apply gf self args)
        (no-method-error name self args))))

(defun install-method-dispatcher (name)
  "Make NAME the function that chooses a method by argument count."
  (unless (get name 'define-method)
    (when (fboundp name)
      (error "DEFINE-METHOD: ~s is already a function or macro, which a ~
              method of that name would replace." name))
    (setf (get name 'define-method) t)
    ;  The hidden names for the first few counts, looked up once.
    (let ((gfs (coerce (loop for n below 8 collect (method-gf-name name n))
                       'simple-vector)))
      (setf (fdefinition name)
            (lambda (self &rest args)
              (declare (dynamic-extent args))
              (let* ((n (length args))
                     (gf (if (< n 8) (svref gfs n) (method-gf-name name n))))
                (if (fboundp gf)
                    (apply (fdefinition gf) self args)
                    (call-variadic-method name self (copy-list args))))))))
  name)

(defun expand-method-call (form name)
  "FORM, a call to NAME, as a direct call to the generic function for its
argument count, when that is known to exist."
  (let* ((args (if (eq (car form) 'funcall) (cddr form) (cdr form)))
         (n (1- (length args))))
    (if (and (>= n 0) (member n (gethash name *method-arities*)))
        (cons (method-gf-name name n) args)
        form)))

(defun method-body (body)
  "BODY marked as a DEFINE-METHOD body; see METHOD-SELF-P.  Declarations
stay at the head, where they belong."
  (let ((decls '()))
    (loop while (and (consp (car body)) (eq (caar body) 'declare))
          do (push (pop body) decls))
    `(symbol-macrolet ((in-define-method t))
       ,@(nreverse decls)
       ,@body)))

(defmacro define-method (method-name class-name arg-list &rest body)
  "Define a method of METHOD-NAME for instances of CLASS-NAME, taking the
arguments in ARG-LIST after the instance, which the body reaches as SELF.

The same name may take a different number of arguments in different classes.
When METHOD-NAME is already a generic function made some other way, such as
by DEFMETHOD, the method is added to it as DEFMETHOD would add it.

SELF is exported by cl3.lisp: a method body written in another package would
otherwise refer to its own SELF rather than this one."
  (if (foreign-generic-function-p method-name)
      `(defmethod ,method-name ((self ,class-name) ,@arg-list)
         ,(method-body body))
      (let* ((arity (and (fixed-arg-list-p arg-list) (length arg-list)))
             (gf (method-gf-name method-name arity))
             (whole (gensym "FORM")))
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (note-method-arity ',method-name ',arity))
           ,(if arity
                `(defmethod ,gf ((self ,class-name) ,@arg-list)
                   ,(method-body body))
                (let ((rest (gensym "ARGS")))
                  `(defmethod ,gf ((self ,class-name) &rest ,rest)
                     (apply #'(lambda (self ,@arg-list) ,(method-body body))
                            self ,rest))))
           ;  A class with no method for this count may have a variadic one.
           ,@(when arity
               (let ((vars (loop repeat arity collect (gensym "ARG"))))
                 `((defmethod ,gf ((self t) ,@vars)
                     (call-variadic-method ',method-name self (list ,@vars))))))
           (install-method-dispatcher ',method-name)
           (define-compiler-macro ,method-name (&whole ,whole &rest args)
             (declare (ignore args))
             (expand-method-call ,whole ',method-name))
           ',method-name))))
