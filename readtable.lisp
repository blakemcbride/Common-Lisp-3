
;  Common Lisp 3 -- case-sensitive symbols
;
;  Loaded last: everything above is read with the standard readtable case,
;  and only user code afterwards is affected.

(in-package :cl3)

; make lisp case sensitive
(setf (readtable-case *readtable*) :invert)
