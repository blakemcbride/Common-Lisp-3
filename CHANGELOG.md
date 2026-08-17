# Changelog

## Unreleased

A correctness release. Both halves of the system were audited against SBCL,
CLISP, ECL, ABCL and CCL: twenty-two defects in the single namespace and six in
the layer over CLOS. All are fixed, each with a regression test. Four appeared
on one implementation only, which is why the suite now runs on all five.

**Not backward compatible with images or fasls built by the previous version.**
See *Upgrading* below.

### Changed — class variables are shared, as in Smalltalk

- **A class variable is one value shared by the class that declares it and
  every subclass**, rather than a separate copy per class. Setting it through
  a subclass sets what the declaring class reads, and the other way round. It
  is still not visible above the class that declares it. The slots holding
  class variables are now `:allocation :class`, which is CLOS's own mechanism
  for exactly this; each class in the hierarchy previously inherited the
  slot's shape but got its own value.

- **An instance reaches its class variables.** `(get-slot instance 'cv)` and
  `(set-slot instance 'cv v)` fall through to the class variables when the
  name is not an instance variable, so a method can use them through `self`
  as in Smalltalk. An instance variable of the same name still takes
  precedence, and a name that is neither is still an error.

- Working around a portability defect while doing so: CLHS 4.3.6 requires the
  value of a slot shared in both the old and new class to be retained when a
  class is redefined. SBCL, CCL and CLISP do; ECL and ABCL lose it, in pure
  CLOS with none of this code involved. `DEFINE-CLASS` now saves the class
  variables before redefining and restores them afterwards.

### Added — class-instance variables

- **`DEFINE-CLASS` takes an optional fifth argument**, a list of
  class-instance variables: declared once, but every class in the hierarchy
  has its own value, for a per-class count or registry. Smalltalk has both
  kinds and now so does this. A four-argument call means exactly what it did
  before, and class variables are unaffected.

### Fixed — the CLOS layer

- **A class can be redefined.** The `DEFCLASS` forms were the initial value of
  a `DEFVAR`, which does not re-evaluate once the variable is bound, so editing
  a class and reloading kept the old definition — while the holder for the
  class variables was reset unconditionally. A redefinition was applied exactly
  backwards: the part that should have persisted was wiped, and the part that
  should have changed was not. The class forms are now always evaluated, and
  the holder is kept so surviving class variables keep their values.

- **A class may have a slot named `class`.** `DEFINE-CLASS` injected a slot of
  that name into every class it made. Nothing ever read it — class variables
  are found through `*CLASS-INSTANCES*` — and it collided with any user slot of
  the same name, which was a duplicate-slot error.

- **Asking a class CLOS-layer questions it cannot answer says so.** `GET-SLOT`
  on a class `DEFINE-CLASS` never made reached `SLOT-VALUE` on `NIL`; it now
  reports that the class has no class variables and names `DEFINE-CLASS`.

- **The parallel class name is interned beside the class name**, rather than in
  whatever package happened to be current when the macro was expanded.

- **The system loads from anywhere.** `cl3.lisp` loaded its parts by bare
  relative name, so `(load "…/cl3")` worked only when the current directory
  happened to be the one holding it. The parts are now resolved relative to
  `cl3.lisp` itself.

- **`DEFINE-METHOD` binds `SELF`, which is now exported.** Unexported, that
  anaphor was a symbol in the `CL3` package, so a method body written anywhere
  else referred to its own `SELF` and the method failed with an unbound
  variable — which is every user of the system, since nobody writes their code
  in `CL3`.

- **`DEFINE-CLASS` and `DEFINE-METHOD` survive conversion.** Written inside a
  `LISP1` form they were macroexpanded, and the `DEFMETHOD` inside was then
  rewritten into `DEFINE-METH` — quietly turning a CLOS method into a
  single-namespace definition. Both now have walkers of their own.

### Fixed — scoping

- **Definitions are lexically scoped.** `DEFINE` expanded to `DEFPARAMETER`,
  which proclaimed the name special globally and permanently, so every later
  `LET` or lambda parameter of that name was a dynamic binding and closures
  captured nothing. A name now holds a hidden cell reached through a symbol
  macro, which a `LET` shadows lexically. `DEFINE-DYNAMIC` gives the old
  behaviour where it is actually wanted.

- **A generic function may have more than one method.** `DEFINE-METH` put the
  call-dispatch macro in the symbol's function cell, so a second method was an
  error — and the failed attempt destroyed the first, leaving the name unbound.

### Fixed — the converter

- **Local bindings shadow Common Lisp names.**
  `(let ((list (lambda (n) (* n 2)))) (list 21))` called `CL:LIST`. The
  converter now carries a lexical environment, checked before anything else.

- **`CASE` and `ECASE` work.** A numeric key aborted macroexpansion; a list of
  keys silently selected the wrong branch.

- **`FLET`, `LABELS`, `DESTRUCTURING-BIND`, `LOOP` destructuring, method
  qualifiers and `DEFINE-MACRO` are no longer corrupted.** All came from one
  assumption: that every argument of an unrecognised form is an expression.
  Forms whose subforms are not all expressions now have walkers; anything else
  is macroexpanded and its expansion converted; anything that can be neither is
  left alone with a style warning.

- **`&optional` and `&key` initialisation forms are converted**, in the
  environment of the parameters preceding them.

- **A definition written inside a binding form sees that binding.**

- **Non-symbol atoms in head position** no longer raise a type error.

- **Symbol identity, not name.** A package that shadows a `COMMON-LISP` name is
  no longer mistaken for it.

- **`DEFUN` inside `LISP1`** produces a callable name.

- **A method body can call `CALL-NEXT-METHOD`.** CLOS makes it and
  `NEXT-METHOD-P` available inside a method, but they are not globally
  `FBOUND`, so the converter rewrote `(call-next-method)` into a reference to a
  variable that does not exist.

- **Back-quote is handled the same way everywhere.** The reader's marker symbol
  was probed for, which is not portable: on CCL the probe returned `CL:LIST`,
  so no `(list …)` form anywhere was converted, and on ABCL splicing templates
  failed outright. There is no probe now. A template's unquoted parts are
  converted in the environment of the call site, so a template may name a
  function only lexically bound where the macro is used.

- **`LOOP` has a walker** rather than being macroexpanded, whose expansion is
  full of implementation internals.

### Fixed — interface

- **`CL3` no longer shadows `LOAD`, `COMPILE-FILE` and `EVAL`,** and no longer
  ships its own. Stock `CL:LOAD` and `CL:COMPILE-FILE` handle the source. Use
  `LISP1` to wrap a bare top level expression over a local function value, and
  `CL3:EVAL-FORM` to convert and evaluate one form.

- **The REPL is usable.** End of input dropped into the debugger rather than
  exiting; a bare `quit` raised an unbound-variable error; `(1 2)` raised a type
  error; and any error unwound out of the loop, ending the session. It now
  reports and carries on, keeps the standard history variables, and prints all
  values.

- **Exit works on every implementation.** `REPQ` had branches for SBCL and
  CLISP only. `CL3:QUIT-LISP` covers all five and more.

### Added

- `DEFINE-DYNAMIC` — a dynamically scoped global.
- `VALUE-OF`, `FUNCTION-OF`, `(SETF VALUE-OF)`, `TRACE-CALLS`, `UNTRACE-CALLS`
  — reach a definition from ordinary Common Lisp. `#'name` and `CL:TRACE`
  cannot work, because the function cell holds the dispatch macro.
- `DEFINE-FORM-WALKER` — teach the converter a macro of your own whose
  arguments are not all expressions.
- `EVAL-FORM`, and `CONVERT` is exported.
- Compile-time argument-count checking, as a style warning, when a definition
  is written as a literal lambda. Switch off with `*CHECK-ARITY*`.
- `*EXPAND-UNKNOWN-MACROS*`, `*WARN-ON-UNCONVERTED*`, `*REP-DEBUGGER*`,
  `*PROMPT*`.
- A dependency-free regression suite (`make test`) and CI across all five
  implementations. There was no runner before, which is why all of this went
  unnoticed.
- `cl3.asd`, so the system loads with `(asdf:load-system "cl3")` and the suite
  runs with `(asdf:test-system "cl3")`. `(load "cl3")` still works.
- `bench/overhead.lisp` and `make bench`, measuring what the single namespace
  costs per call on the implementation in front of you.
- `test-3.lisp` works again; it required a `LISP1` package this system has
  never had.

### Removed

- `metaclass` from the export list. It named nothing at all — no function,
  variable, macro or class — and being lower case in that list, a user writing
  `metaclass` under the `:invert` readtable would have got a different symbol
  anyway.

### Upgrading

1. **Recompile from source, in a fresh image.** Fasls built by the previous
   version contain `DEFPARAMETER`-based definitions.

2. **A name that is already special cannot be defined.** `DEFINE-SYMBOL-MACRO`
   signals on an already-special symbol, so loading new-style code into an
   image where the old `DEFINE` — or any `DEFVAR` or `DEFPARAMETER` — has
   touched those names fails. The error says so and points at
   `DEFINE-DYNAMIC`.

3. **Code that relied on dynamically rebinding a defined name changes
   meaning.** That is the point; `DEFINE-DYNAMIC` is the escape hatch.

4. **`SYMBOL-VALUE` no longer reaches a defined name**, which is no longer
   special. Use `CL3:VALUE-OF`.

5. **Compiled code now needs the `CL3-CELLS` package** to exist at load time,
   alongside `CL3`. See the preamble in test-3.lisp.

6. **The retired shadows survive in a stale image.** `DEFPACKAGE` does not
   remove shadowing symbols a package already has, so loading this into an
   image where the previous version was loaded leaves `CL3::LOAD`,
   `CL3::COMPILE-FILE` and `CL3::EVAL` shadowing `COMMON-LISP`, and the fix not
   in effect. A fresh image is required in any case.
