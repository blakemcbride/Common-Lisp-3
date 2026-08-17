
;  Common Lisp 3
;
;  ANSI Common Lisp with a single namespace, a layer over CLOS, and
;  case-sensitive symbols.  See README.md.
;
;  This is the entry point for loading without ASDF:
;
;      (load "cl3")
;      (use-package :cl3)
;
;  ASDF users want (asdf:load-system "cl3") instead, which loads the same
;  parts as compiled components; see cl3.asd.
;
;  The parts are resolved relative to this file rather than to the current
;  directory, so that CL3 can be loaded from anywhere.

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:let ((here (cl:make-pathname
                  :name cl:nil :type cl:nil :version cl:nil
                  :defaults (cl:or cl:*load-truename*
                                   cl:*compile-file-truename*
                                   cl:*default-pathname-defaults*))))
    (cl:dolist (part '("package" "clos-utils" "lisp1" "readtable"))
      (cl:load (cl:merge-pathnames part here)))))
