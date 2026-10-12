;;;; program.lisp -- a whole program in ordinary Common Lisp against the same
;;;; program in Common Lisp 3.
;;;;
;;;;     sbcl --script bench/program.lisp
;;;;
;;;; overhead.lisp isolates the cost of one call.  This measures what a
;;;; program pays, workload by workload: recursion, higher-order functions and
;;;; closures, sorting with a predicate, and objects.  The objects workload
;;;; compares each dialect's own idiom -- DEFCLASS accessors against
;;;; DEFINE-CLASS with GET-SLOT, SET-SLOT and DEFINE-METHOD -- so it measures
;;;; the layer over CLOS as well as the single namespace.
;;;;
;;;; Both programs are compiled with COMPILE-FILE and loaded from the fasl.

(in-package "COMMON-LISP-USER")

(defparameter *bench-dir*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *root-dir*
  (make-pathname :directory (butlast (pathname-directory *bench-dir*))
                 :defaults *bench-dir*))

(defparameter *repetitions* 5
  "Each workload runs this many times; the fastest run is reported.")

(defun quietly-load (name)
  "Compile NAME, in the bench directory, and load the result."
  (let ((source (merge-pathnames name *bench-dir*)))
    (handler-bind ((warning #'muffle-warning))
      (let ((*compile-verbose* nil) (*compile-print* nil)
            (*error-output* (make-broadcast-stream)))
        (load (compile-file source))))))

(let ((*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "cl3.lisp" *root-dir*))))

;; cl3 sets the readtable to :INVERT.  Both programs are written in lower
;; case, so they read identically either way.
(quietly-load "program-plain.lisp")
(quietly-load "program-cl3.lisp")

(defun sym (package name) (find-symbol name package))

(defun call (package name &rest args)
  "Call NAME in PACKAGE.  A CL3 name holds its function as a value."
  (let ((s (sym package name)))
    (apply (if (string= package "BENCH-CL3")
               (funcall (sym "CL3" "FUNCTION-OF") s)
               (symbol-function s))
           args)))

(defparameter *data*
  (let ((state (sb-ext:seed-random-state 42)))
    (loop repeat 200000 collect (random 1000000 state))))

(defparameter *workloads*
  ;; (label setup run) -- SETUP builds fresh input outside the timing; RUN is
  ;; timed and returns a result that both dialects must agree on.
  (list
   (list "fib 37" (lambda (p) (declare (ignore p)) nil)
         (lambda (p x) (declare (ignore x)) (call p "FIB" 37)))
   (list "map/reduce/compose, 1M x 5"
         (lambda (p) (declare (ignore p)) (loop for i below 1000000 collect i))
         (lambda (p x) (let (r) (dotimes (k 5 r) (setq r (call p "HIGHER-ORDER" x))))))
   (list "merge sort 200K x 5"
         (lambda (p) (declare (ignore p)) nil)
         (lambda (p x) (declare (ignore x))
           (let (r)
             (dotimes (k 5 (reduce #'+ (subseq r 0 100)))
               (setq r (call p "MERGE-SORT" (copy-list *data*)
                             (if (string= p "BENCH-CL3")
                                 (funcall (sym "CL3" "VALUE-OF") (sym p "NUM-LESS"))
                                 (symbol-function (sym p "NUM-LESS")))))))))
   (list "objects 10K accts x 500"
         (lambda (p) (call p "MAKE-ACCOUNTS" 10000))
         (lambda (p x) (call p "OBJECTS" x 500)))))

(defun timed (package setup run)
  (let ((best nil) (result nil))
    (dotimes (i *repetitions*)
      (let ((input (funcall setup package)))
        (sb-ext:gc :full t)
        (let ((start (get-internal-real-time)))
          (setq result (funcall run package input))
          (let ((s (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
            (when (or (null best) (< s best)) (setq best s))))))
    (values (float best) result)))

(defun run ()
  (format t "~&~%Plain Common Lisp vs Common Lisp 3 -- ~a ~a~%"
          (lisp-implementation-type) (lisp-implementation-version))
  (format t "~v,,,'-<~>~%" 72)
  (format t "  best of ~d, seconds~%~%" *repetitions*)
  (format t "  ~30a ~9@a ~9@a ~8@a~%" "workload" "plain" "CL3" "CL3/plain")
  (let ((tp 0) (tc 0))
    (dolist (w *workloads*)
      (destructuring-bind (label setup run) w
        (multiple-value-bind (p pr) (timed "BENCH-PLAIN" setup run)
          (multiple-value-bind (c cr) (timed "BENCH-CL3" setup run)
            (unless (equal pr cr)
              (format t "  ** ~a: results differ: ~s vs ~s~%" label pr cr))
            (incf tp p) (incf tc c)
            (format t "  ~30a ~9,3f ~9,3f ~8,2fx~%" label p c (/ c p))))))
    (format t "  ~30a ~9,3f ~9,3f ~8,2fx~%~%" "total" tp tc (/ tc tp)))
  (finish-output))

(run)
(sb-ext:exit :code 0)
