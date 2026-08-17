
# Common Lisp 3

This repository represents the author's view on a new dialect
of Common Lisp called Common Lisp 3.  It is an effort to modernize and 
clean up the ANSI Common Lisp standard.  

Common Lisp 3 consists of ANSI Common Lisp with the following changes:

1. The function and variable namespaces have been collapsed into a single
namespace. This simplifies the language and makes it easier to perform
functional programming.

2. Although CLOS is untouched, Common Lisp 3 adds a layer on top of CLOS
that facilitates the creation of object-oriented code.  This makes it
easier to do the things that most programmers do most of the time.

3. Common Lisp 3 supports case-sensitive symbols.

4. Common Lisp 3 supports native threads.

5. Common Lisp 3 supports tail recursion elimination.

This repository contains code that adds items 1, 2, and 3 to ANSI
Common Lisp.

Items 4 and 5 are already a part of the SBCL Common Lisp implementation.

## Usage

Although this package works correctly in most versions of Common Lisp,
you would not get the native threads and tail recursion enhancements
offered by SBCL.  SBCL is therefore the recommended implementation.

The system can be loaded with the following Lisp commands:

```
(load "cl3")
(use-package :cl3)
```

or, with ASDF:

```
(asdf:load-system "cl3")
(use-package :cl3)
```

The three changes are separable.  To take the single namespace on its
own -- no layer over CLOS, and symbols left case-insensitive as in
ordinary Common Lisp -- load just these two:

```
(load "package")
(load "lisp1")
(use-package :cl3)
```

`DEFINE` gives a name a value, which may be a function:

```lisp
(define add (lambda (a b) (+ a b)))
(define apply-to (lambda (f a b) (f a b)))
(apply-to add 5 6)                                ; => 11
```

Definitions are lexically scoped, so closures behave:

```lisp
(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda () (setq count (+ count 1)) count))))
```

`DEFINE-METH` adds methods to a generic function held in such a name, and
is not the same thing as `DEFINE-METHOD`, which is the fixed-argument
method form belonging to the layer over CLOS.

A bare top level expression over a local function value needs `LISP1`
around it; definitions and calls to defined names do not:

```lisp
(lisp1 (let ((f (lambda (x) (* x 2)))) (print (f 3))))
```

See lisp1.txt for the single namespace in detail, test-3.lisp for
examples of it, and test-2.lisp for the layer over CLOS.

## Portability

Tested, on every commit, against:

* SBCL
* CLISP
* ABCL
* CCL
* ECL

MKCL is believed to work but is not in the test matrix.

## Testing

```
make test
```

runs the regression suite on whichever of those implementations are
installed.  Run it on all of them rather than the one to hand:
implementations differ in ways that matter here, particularly in how the
reader represents back-quote and when a compile-time macro definition
becomes visible.

`make bench` measures what the single namespace costs per call.  A
call through a name compiles to a `funcall` of a variable, which runs
about 1.0x-1.6x a native call depending on the implementation.

## Needed

It is known that a comprehensive manual is needed.

## Source

The source code for this system is located at [https://github.com/blakemcbride/common-lisp-3](https://github.com/blakemcbride/common-lisp-3)

It was written by Blake McBride (blake@mcbridemail.com)
