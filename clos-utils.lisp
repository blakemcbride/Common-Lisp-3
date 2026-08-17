
;  Common Lisp CLOS utilities
;
;  Written by:
;        Blake McBride
;        blake@mcbridemail.com
;
;  A layer over CLOS giving classes their own class variables -- slots
;  belonging to the class rather than to its instances.
;
;  Each class defined here gets a parallel class, CLASS-<name>, whose
;  hierarchy mirrors the primary one and whose slots are the class
;  variables.  One instance of that parallel class holds the values, and
;  *CLASS-INSTANCES* maps the primary class object to it.
;
;  Smalltalk has two kinds of class-side state, and so does this:
;
;    class variables           one value, shared by the class that declares
;                              it and every subclass.  Setting it through a
;                              subclass, or through an instance, sets the
;                              value the whole hierarchy sees.  Held in a
;                              :allocation :class slot of the parallel
;                              class, which is what makes it one location.
;
;    class-instance variables  declared once, but every class in the
;                              hierarchy gets its own value -- a per-class
;                              counter or registry.  Held in an ordinary
;                              slot of the parallel class, of which there
;                              is one instance per class.
;
;  Neither is visible above the class that declares it.
;
;  The parallel class exists so that a class variable can be read from a
;  class object with no instance in hand.  Reaching a :allocation :class
;  slot directly from a class needs CLASS-PROTOTYPE, which is MOP rather
;  than ANSI; an inert instance is portable.  It also keeps the user's
;  INITIALIZE-INSTANCE methods from firing when a class is defined.
;

(in-package :cl3)

(defvar *class-instances* (make-hash-table :test #'eq)
  "Primary class object -> the instance holding its class variables.")

(defun parallel-class-name (name)
  "The name of the parallel class holding NAME's class variables.

Interned beside NAME rather than in whatever package happens to be current
when the macro is expanded, so that defining a class from another package
does not scatter the parallel names."
  (intern (concatenate 'string "CLASS-" (string name))
          (or (and (symbolp name) (symbol-package name)) *package*)))

(defmacro define-class (name super-class-list class-variables instance-variables
                        &optional class-instance-variables)
  "Define a class, its parallel class-variable class, and the holder for them.

CLASS-VARIABLES are shared with every subclass: one value for the hierarchy.
CLASS-INSTANCE-VARIABLES are declared once but give every class its own
value.  Both are read and written with GET-SLOT and SET-SLOT on the class,
and reached from an instance when the name is not an instance variable.
The fifth argument is optional, so a four-argument call means what it always
did.

Re-evaluating this redefines the class.  The DEFCLASS forms are evaluated
every time -- they used to be the initial value of a DEFVAR, which does not
re-evaluate once the variable is bound, so editing a class and reloading
silently kept the old definition while resetting its class variables.  The
holder is now kept across a redefinition too: CLOS updates it for the new
slots, and the values of the class variables that survive the change survive
with it."
  (let ((parallel (parallel-class-name name))
        (parallel-supers (mapcar #'parallel-class-name super-class-list)))
    `(progn
       ;  Captured before the DEFCLASS below, restored after it; see
       ;  *SAVED-CLASS-VARIABLES*.
       (setf (gethash ',name *saved-class-variables*)
             (class-variable-values (find-class ',name nil)
                                    ',(class-variable-names class-variables)))
       (defclass ,parallel ,parallel-supers
         ,(append (mapcar #'shared-slot-spec class-variables)
                  (mapcar #'per-class-slot-spec class-instance-variables)))
       (defclass ,name ,super-class-list ,instance-variables)
       (defparameter ,parallel (find-class ',parallel))
       (defparameter ,name (find-class ',name))
       (unless (gethash ,name *class-instances*)
         (setf (gethash ,name *class-instances*)
               (make-instance (find-class ',parallel))))
       (restore-class-variables ,name (gethash ',name *saved-class-variables*))
       (remhash ',name *saved-class-variables*)
       ,name)))

(defvar *saved-class-variables* (make-hash-table :test #'eq)
  "Class name -> values captured just before a redefinition.

CLHS 4.3.6 says the value of a slot shared in both the old and the new class
is retained when a class is redefined.  SBCL, CCL and CLISP do that; ECL and
ABCL lose it, in pure CLOS with none of this code involved.  DEFINE-CLASS
therefore saves the class variables before redefining and puts them back
afterwards, so a class variable survives a redefinition everywhere.")

(defun shared-slot-spec (spec)
  "SPEC as a slot of the parallel class holding a class variable: one
location for the whole hierarchy.  An explicit :allocation is left alone, so
a caller who knows what the parallel class is can say what they mean."
  (let ((spec (if (consp spec) spec (list spec))))
    (if (member :allocation spec)
        spec
        (append spec '(:allocation :class)))))

(defun per-class-slot-spec (spec)
  "SPEC as a slot of the parallel class holding a class-instance variable:
one location per class, since there is one parallel instance per class."
  (if (consp spec) spec (list spec)))

(defun class-variable-names (specs)
  "The names in a list of slot specifications."
  (mapcar (lambda (spec) (if (consp spec) (car spec) spec)) specs))

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

(defun get-slot (obj slot)
  "The value of SLOT in OBJ, which may be an instance or a class.

An instance reaches its own instance variables first and then its class
variables, so that a method can read either through SELF, as in Smalltalk.
An instance variable shadows a class variable of the same name."
  (cond ((typep obj 'standard-class) (slot-value (get-class-object obj) slot))
        ((slot-exists-p obj slot)    (slot-value obj slot))
        (t (slot-value (get-class-object (class-of obj)) slot))))

(defun set-slot (obj slot val)
  "Set SLOT in OBJ to VAL.  Resolved as GET-SLOT resolves it, so setting a
class variable through an instance sets the one value the class shares."
  (cond ((typep obj 'standard-class)
         (setf (slot-value (get-class-object obj) slot) val))
        ((slot-exists-p obj slot)
         (setf (slot-value obj slot) val))
        (t (setf (slot-value (get-class-object (class-of obj)) slot) val))))

(defmacro define-method (method-name class-name arg-list &rest body)
  "define-method defines a fixed argument method and associates it to a variable argument generic.
   This allows the same method name in different classes to have a different number of fixed arguments.

   The instance is bound to SELF, which cl3.lisp exports for that reason: a
   method body written in another package would otherwise refer to its own
   SELF rather than this one."
  (let ((alist (gensym)))
    `(defmethod ,method-name ((self ,class-name) &rest ,alist)
       (apply #'(lambda (self ,@arg-list) ,@body)
	      (cons self ,alist)))))
