
(load "cl3")
(use-package :cl3)


(defmacro testr (correct-result s-exp)
  "test an expression and then compare its result against an expected result"
  `(progn
     (format t "~a: " ',s-exp)
     (handler-case
         (let ((result (eval ',s-exp)))
	   (format t "~a " result)
           (if (equal result ,correct-result)
               (format t "(correct)~%")
               (format t "(wrong)~%")))
       (error ()
         (format t "exception~%")))))

(defmacro test (s-exp)
  "test an expression without expecting a particular result"
  `(progn
     (format t "~a: " ',s-exp)
     (handler-case
         (let ((result (eval ',s-exp)))
           (format t "~a~%" result))
       (error ()
         (format t "exception~%")))))

(defmacro testn (s-exp)
  "test an expression without expecting a particular result and don't display the result"
  `(progn
     (format t "~a~%" ',s-exp)
     (handler-case
	 (eval ',s-exp)
       (error ()
         (format t "exception~%")))))

(defmacro teste (s-exp)
  "test an expression; expect an exception"
  `(progn
     (format t "~a: " ',s-exp)
     (handler-case
         (let ((result (eval ',s-exp)))
           (format t "~a (unexpected)~%" result))
       (error ()
         (format t "exception (expected)~%")))))

(test (define-class my-class () (cv1 cv2) (iv1 iv2)))

(test (defvar i1 (make-instance my-class)))

(testr 44 (set-slot my-class 'cv1 44))

(testr 77 (set-slot i1 'iv1 77))

(testr 44 (get-slot my-class 'cv1))

(testr 77 (get-slot i1 'iv1))

(test (define-class my-class2 (my-class) (cv3) (iv3)))

(test (defvar i2 (make-instance my-class2)))

; A class variable is one storage location shared with every subclass, as in
; Smalltalk, so the value set on my-class is what my-class2 sees.
(testr 44 (get-slot my-class2 'cv1))

; and setting it through the subclass sets the one value both share
(testr 42 (set-slot my-class2 'cv1 42))

(testr 42 (get-slot my-class2 'cv1))

(testr 42 (get-slot my-class 'cv1))

; an instance reaches its class variables too
(testr 42 (get-slot i1 'cv1))

; instance method
(testn  (defmethod addv ((ins my-class) val)
	 (+ (get-slot ins 'iv1) val)))

; class method
(testn  (defmethod addv ((cls (eql my-class)) val)
	 (+ (get-slot cls 'cv1) val)))

(testr 177 (addv i1 100))
; cv1 is 42 here, not 44: the subclass set the one value both share
(testr 142 (addv my-class 100))

(testr 1000 (set-slot i2 'iv1 1000))
(testr 1000 (get-slot i2 'iv1))
(testr 1005 (addv i2 5))

(test (define var1 'abc))
(test (define VAR1 'def))
(testr 'abc var1)
(testr 'def VAR1)

(test (define add50 (lambda (n) (+ n 50))))

(testr 75 (add50 25))
