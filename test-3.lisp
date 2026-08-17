
; Lisp1 examples
;
; Try to execute the stuff in comments


(eval-when (:compile-toplevel :execute)
  (require :cl3 "cl3")
  (use-package :cl3))

;  Compiled code does not need cl3 loaded in order to run, but these
;  packages must exist, because the compiled code refers to symbols in
;  them.  CL3-CELLS holds the value cell behind each global lexical
;  created by DEFINE, and the generic function behind each DEFINE-METH.
(eval-when (:load-toplevel)
  (dolist (name '("CL3" "CL3-CELLS"))
    (if (not (find-package name))
        (make-package name :use '()))))

(define add
    (lambda (a b)
      (+ a b)))

(define var1 add)

(define var2 5)

(define var3 6)

;  var1
;  var2
;  (var1 var2 var3)

(define fun1
    (lambda (a b)
      (var1 a b)))

;  (fun1 7 8)

(define fun2
  (lambda (fun)
    (fun 5 6)))

; (fun2 var1)
; (fun2 fun1)
; (fun2 (lambda (a b) (* a b)))

(define fun3
    (lambda (a)
      (let ((fun2 (lambda (a b) (* a b)))
	    (c 100))
	(fun2 a c))))

; (fun3 22)

(define-macro mac (a b)
  `(var1 ,a , b))

; (mac 3 5)

(define fun4
    (lambda (x)
      (lambda (y)
	(+ x y))))

; (lisp1 ((fun4 1) 2))


;  ------------------------------------------------------------------
;  Lexical scoping: each counter has its own COUNT, and the global one
;  below is untouched.

(define count-of-calls 0)

(define make-counter
    (lambda ()
      (let ((count-of-calls 0))
        (lambda ()
          (setq count-of-calls (+ count-of-calls 1))
          count-of-calls))))

; (define c1 (make-counter))
; (define c2 (make-counter))
; (c1) (c1) (c2)   =>  1  2  1
; count-of-calls   =>  0

;  DEFINE-METH puts several methods on one generic function, qualifiers
;  included.  (DEFINE-METHOD is the layer over CLOS; see test-2.lisp.)

(define-meth area ((s integer)) (* s s))
(define-meth area ((s list))    (* (first s) (second s)))
(define-meth area :around ((s integer)) (if (< s 0) 0 (call-next-method)))

; (area 5)       =>  25
; (area '(3 4))  =>  12
; (area -5)      =>  0

;  #'ADD cannot work -- ADD's function cell holds the macro that makes
;  (ADD 1 2) a call -- but the value is a variable, so these do:

; (funcall add 1 2)            =>  3
; (mapcar add '(1 2) '(10 20)) =>  (11 22)
; (function-of 'add)           =>  #<function>
; (trace-calls 'add) (add 1 2) (untrace-calls 'add)
