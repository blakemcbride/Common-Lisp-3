;;;; program-plain.lisp -- the benchmark program in ordinary Common Lisp.
;;;;
;;;; program-cl3.lisp is the same program written in Common Lisp 3.  The two
;;;; are kept line for line alike so that only the dialect differs.  Run both
;;;; with bench/program.lisp.

(defpackage "BENCH-PLAIN" (:use "COMMON-LISP"))
(in-package "BENCH-PLAIN")

;;; 1. Recursion through a global name.

(defun fib (n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

;;; 2. Higher-order functions and closures.

(defun square (x) (* x x))
(defun inc (x) (+ x 1))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun my-map (f list)
  (let ((r '()))
    (dolist (x list (nreverse r))
      (push (funcall f x) r))))

(defun my-reduce (f init list)
  (let ((acc init))
    (dolist (x list acc)
      (setq acc (funcall f acc x)))))

(defun higher-order (list)
  (my-reduce #'+ 0 (my-map (compose #'square #'inc) list)))

;;; 3. Sorting with a predicate passed in.

(defun merge-lists (a b less)
  (let* ((head (list nil)) (tail head))
    (loop
      (cond ((null a) (setf (cdr tail) b) (return (cdr head)))
            ((null b) (setf (cdr tail) a) (return (cdr head)))
            ((funcall less (car b) (car a))
             (setf (cdr tail) b tail b b (cdr b)))
            (t
             (setf (cdr tail) a tail a a (cdr a)))))))

(defun merge-sort (list less)
  (if (or (null list) (null (cdr list)))
      list
      (let* ((half (floor (length list) 2))
             (back (nthcdr half list))
             (front (ldiff list back)))
        (merge-lists (merge-sort front less) (merge-sort back less) less))))

(defun num-less (a b) (< a b))

;;; 4. Objects: a class with an instance variable and a class variable.

(defclass account ()
  ((balance :initform 0 :accessor balance)
   (rate :allocation :class :initform 1 :accessor rate)))

(defmethod deposit ((a account) amount)
  (setf (balance a) (+ (balance a) amount)))

(defmethod accrue ((a account))
  (setf (balance a) (+ (balance a) (rate a))))

(defun objects (accounts rounds)
  (dotimes (r rounds)
    (dolist (a accounts)
      (deposit a r)
      (accrue a)))
  (let ((total 0))
    (dolist (a accounts total)
      (setq total (+ total (balance a))))))

(defun make-accounts (n)
  (loop repeat n collect (make-instance 'account)))
