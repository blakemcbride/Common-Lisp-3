;;;; program-cl3.lisp -- the benchmark program in Common Lisp 3.
;;;;
;;;; program-plain.lisp is the same program in ordinary Common Lisp.  The two
;;;; are kept line for line alike so that only the dialect differs.  Run both
;;;; with bench/program.lisp, which loads cl3 before this file.

(defpackage "BENCH-CL3" (:use "COMMON-LISP" "CL3"))
(in-package "BENCH-CL3")

;;; 1. Recursion through a global name.

(define fib
  (lambda (n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

;;; 2. Higher-order functions and closures.

(define square (lambda (x) (* x x)))
(define inc (lambda (x) (+ x 1)))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define my-map
  (lambda (f list)
    (let ((r '()))
      (dolist (x list (nreverse r))
        (push (f x) r)))))

(define my-reduce
  (lambda (f init list)
    (let ((acc init))
      (dolist (x list acc)
        (setq acc (f acc x))))))

(define higher-order
  (lambda (list)
    (my-reduce + 0 (my-map (compose square inc) list))))

;;; 3. Sorting with a predicate passed in.

(define merge-lists
  (lambda (a b less)
    (let* ((head (list nil)) (tail head))
      (loop
        (cond ((null a) (setf (cdr tail) b) (return (cdr head)))
              ((null b) (setf (cdr tail) a) (return (cdr head)))
              ((less (car b) (car a))
               (setf (cdr tail) b tail b b (cdr b)))
              (t
               (setf (cdr tail) a tail a a (cdr a))))))))

(define merge-sort
  (lambda (list less)
    (if (or (null list) (null (cdr list)))
        list
        (let* ((half (floor (length list) 2))
               (back (nthcdr half list))
               (front (ldiff list back)))
          (merge-lists (merge-sort front less) (merge-sort back less) less)))))

(define num-less (lambda (a b) (< a b)))

;;; 4. Objects: a class with an instance variable and a class variable.

(define-class account () (rate) ((balance :initform 0)))
(set-slot account 'rate 1)

(define-method deposit account (amount)
  (set-slot self 'balance (+ (get-slot self 'balance) amount)))

(define-method accrue account ()
  (set-slot self 'balance (+ (get-slot self 'balance) (get-slot self 'rate))))

(define objects
  (lambda (accounts rounds)
    (dotimes (r rounds)
      (dolist (a accounts)
        (deposit a r)
        (accrue a)))
    (let ((total 0))
      (dolist (a accounts total)
        (setq total (+ total (get-slot a 'balance)))))))

(define make-accounts
  (lambda (n)
    (loop repeat n collect (make-instance account))))
