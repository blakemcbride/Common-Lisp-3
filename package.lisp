
;  Common Lisp 3 -- package definitions
;
;  Separated from cl3.lisp so that the parts can be listed as ASDF
;  components in dependency order.  cl3.lisp remains the entry point for
;  loading without ASDF.

(defpackage :cl3
  (:use "COMMON-LISP")
  (:export
   ;  definition -- the single namespace
   "DEFINE" "DEFINE-DYNAMIC" "DEFINE-MACRO" "DEFINE-METH"
   ;  converting code
   "LISP1" "CONVERT" "EVAL-FORM" "DEFINE-FORM-WALKER"
   ;  reaching a definition from ordinary Common Lisp
   "VALUE-OF" "FUNCTION-OF" "TRACE-CALLS" "UNTRACE-CALLS"
   ;  controls
   "*EXPAND-UNKNOWN-MACROS*" "*WARN-ON-UNCONVERTED*" "UNCONVERTED-FORM"
   "*CHECK-ARITY*" "ARITY-MISMATCH"
   ;  interactive
   "REP" "REPQ" "QUIT-LISP" "*PROMPT*" "*REP-DEBUGGER*"
   ;  the layer over CLOS.  SELF is the anaphor DEFINE-METHOD binds to the
   ;  instance, and has to be exported: unexported, a method body written in
   ;  any other package refers to its own SELF, not the one the macro bound,
   ;  and the method fails with an unbound variable.
   ;  "metaclass" was exported here but named nothing at all -- no function,
   ;  variable, macro or class -- and being lower case in this list, a user
   ;  writing metaclass under the :invert readtable below would have got a
   ;  different symbol anyway.  Removed.
   "DEFINE-CLASS" "DEFINE-METHOD" "SELF" "SET-SLOT" "GET-SLOT"))

;  Private home for the value cells behind global lexicals, and for the
;  generic functions behind DEFINE-METH.  Nothing is interned here by hand;
;  see HIDDEN-SYMBOL.  Compiled user code refers to these symbols, so this
;  package must exist in order to load such a file.
(defpackage "CL3-CELLS" (:use))

(provide :cl3)
