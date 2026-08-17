;;;; overhead.lisp -- per-call cost of a single-namespace call versus a native CL call.
;;;;
;;;;     make bench                    every installed implementation
;;;;     sbcl --script bench/overhead.lisp
;;;;
;;;; lisp1.txt used to claim the overhead was "very, very minimal or totally
;;;; non-existent".  This measures it, so the claim can be stated in numbers and
;;;; kept honest as the converter grows.
;;;;
;;;; A single-namespace call compiles to (FUNCALL <variable> args...): a variable read
;;;; plus an indirect call, opaque to the compiler.  The cycles are only part
;;;; of the cost -- the call is also invisible to arity checking, inlining and
;;;; type inference, which no timing loop can show.
;;;;
;;;; Caveats: a microbenchmark on a trivial callee exaggerates call overhead
;;;; relative to real code, and ABCL's JIT makes its numbers unstable enough
;;;; that they should not be quoted.

(in-package "COMMON-LISP-USER")

(defparameter *bench-dir*
  (make-pathname :name nil :type nil :version nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *root-dir*
  (make-pathname :directory (butlast (pathname-directory *bench-dir*))
                 :defaults *bench-dir*))

(load (merge-pathnames "cl3.lisp" *root-dir*))
(use-package :cl3)

(defparameter *iterations* 20000000)
(defparameter *repetitions* 3
  "Each variant runs this many times; the fastest run is reported, which
suppresses scheduler and GC noise better than an average.")

;;; The callee, defined three ways.  Arguments depend on the loop variable so
;;; that no implementation can constant-fold the call away.

(defun native-add (a b) (+ a b))

(defun native-add-notinline (a b) (+ a b))
(declaim (notinline native-add-notinline))

(define lisp1-add (lambda (a b) (+ a b)))

(defun bench-native (n)
  (let ((acc 0)) (dotimes (i n acc) (setq acc (native-add i 1)))))

(defun bench-native-notinline (n)
  (let ((acc 0)) (dotimes (i n acc) (setq acc (native-add-notinline i 1)))))

(defun bench-lisp1 (n)
  (let ((acc 0)) (dotimes (i n acc) (setq acc (lisp1-add i 1)))))

(defun bench-empty (n)
  (let ((acc 0)) (dotimes (i n acc) (setq acc i))))

(dolist (f '(bench-native bench-native-notinline bench-lisp1 bench-empty))
  (unless (compiled-function-p (symbol-function f))
    (compile f)))

(defun timed (fn n)
  (let ((best nil))
    (dotimes (r *repetitions* best)
      (declare (ignore r))
      (let ((start (get-internal-real-time)))
        (funcall fn n)
        (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))
          (when (or (null best) (< elapsed best)) (setq best elapsed)))))))

(defun run ()
  (format t "~&~%Common Lisp 3 call overhead -- ~a ~a~%"
          (lisp-implementation-type) (lisp-implementation-version))
  (format t "~v,,,'-<~>~%" 68)
  (format t "  ~d iterations, best of ~d~%~%" *iterations* *repetitions*)

  ;; Warm up: first call triggers JIT and page faults on some implementations.
  (dolist (f '(bench-native bench-native-notinline bench-lisp1 bench-empty))
    (funcall f 100000))

  (let* ((loop-only (timed #'bench-empty *iterations*))
         (rows (list (list "loop only (baseline)"     loop-only)
                     (list "native call"              (timed #'bench-native *iterations*))
                     (list "native call (notinline)"  (timed #'bench-native-notinline *iterations*))
                     (list "single-namespace call"    (timed #'bench-lisp1 *iterations*))))
         (native (second (assoc "native call" rows :test #'string=)))
         (l1     (second (assoc "single-namespace call"  rows :test #'string=))))
    (format t "  ~28a ~10a ~10a~%" "variant" "seconds" "vs native")
    (dolist (row rows)
      (destructuring-bind (label secs) row
        (format t "  ~28a ~10,3f ~@[~10,2f~]~%" label (float secs)
                (when (and (plusp native) (not (string= label "loop only (baseline)")))
                  (float (/ secs native))))))
    (format t "~%")
    ;; Subtract the empty loop so the figure reflects the calls themselves.
    ;; Only when that subtraction leaves enough to divide by: where a native
    ;; call costs barely more than the bare loop, the remainder is mostly
    ;; noise and the ratio computed from it is meaningless.
    (let ((n (- native loop-only))
          (m (- l1 loop-only)))
      (if (> n (* 0.5 loop-only))
          (format t "  Call cost with loop overhead removed: the single-namespace call is ~,2fx a native call.~%"
                  (float (/ m n)))
          (format t "  Loop overhead dominates the native call here (~,3f vs ~,3f);~%~
                     ~&  the subtracted ratio would be noise, so only the column above~%~
                     ~&  is meaningful on this implementation.~%"
                  (float loop-only) (float native))))
    (format t "  Microbenchmark on a trivial callee. Does not capture the loss of~%")
    (format t "  compile-time arity checking, inlining or type inference.~%~%"))
  (finish-output))

(run)

#+sbcl  (sb-ext:exit :code 0)
#+ccl   (ccl:quit 0)
#+ecl   (ext:quit 0)
#+clisp (ext:quit 0)
#+abcl  (ext:quit :status 0)
