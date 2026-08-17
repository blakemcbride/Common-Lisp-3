
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
;  *CLASS-INSTANCES* maps the primary class object to it.  The existence of
;  a class variable is therefore inherited, while its value is not: each
;  class in the hierarchy has its own parallel instance.
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

(defmacro define-class (name super-class-list class-variables instance-variables)
  "Define a class, its parallel class-variable class, and the holder for them.

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
       (defclass ,parallel ,parallel-supers ,class-variables)
       (defclass ,name ,super-class-list ,instance-variables)
       (defparameter ,parallel (find-class ',parallel))
       (defparameter ,name (find-class ',name))
       (unless (gethash ,name *class-instances*)
         (setf (gethash ,name *class-instances*)
               (make-instance (find-class ',parallel))))
       ,name)))

(defun get-class-object (cls)
  "The instance holding CLS's class variables."
  (or (gethash cls *class-instances*)
      (error "~s has no class variables: it was not defined by DEFINE-CLASS."
             cls)))

(defun get-slot (obj slot)
  (if (typep obj 'standard-class)
      (slot-value (get-class-object obj) slot)
      (slot-value obj slot)))

(defun set-slot (obj slot val)
  (if (typep obj 'standard-class)
      (setf (slot-value (get-class-object obj) slot) val)
      (setf (slot-value obj slot) val)))

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
