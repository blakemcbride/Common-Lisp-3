# Common Lisp 3 -- test and benchmark driver.
#
#   make test    run the regression suite on every installed implementation
#   make bench   measure per-call overhead on every installed implementation
#   make clean   remove fasls and scratch files
#
# Restrict to one implementation with, for example:
#
#   make test LISPS=sbcl

LISPS ?= sbcl ccl ecl abcl clisp

.PHONY: test bench clean help

help:
	@echo "make test [LISPS='sbcl ccl ...']   run the regression suite"
	@echo "make bench                         measure per-call overhead"
	@echo "make clean                         remove fasls and scratch files"

test:
	@fail=0; found=0; \
	for l in $(LISPS); do \
	  if command -v $$l >/dev/null 2>&1; then \
	    found=1; \
	    $(MAKE) --no-print-directory run-$$l SCRIPT=tests/run.lisp || fail=1; \
	  else \
	    echo "--- $$l not installed, skipping ---"; \
	  fi; \
	done; \
	if [ $$found -eq 0 ]; then \
	  echo "ERROR: none of '$(LISPS)' is installed; nothing was tested."; \
	  exit 1; \
	fi; \
	exit $$fail

bench:
	@for l in $(LISPS); do \
	  if command -v $$l >/dev/null 2>&1; then \
	    $(MAKE) --no-print-directory run-$$l SCRIPT=bench/overhead.lisp || true; \
	  fi; \
	done

# Per-implementation invocation.  Each script exits with its own status.
run-sbcl:
	@sbcl --script $(SCRIPT)
run-ccl:
	@ccl -n -Q -l $(SCRIPT)
run-ecl:
	@ecl -q -norc -load $(SCRIPT)
run-abcl:
	@abcl --noinform --batch --load $(SCRIPT) 2>&1 | grep -v 'Failed to introspect'
run-clisp:
	@clisp -q -norc $(SCRIPT)

clean:
	@rm -rf tests/scratch
	@find . -name '*.fasl'  -delete
	@find . -name '*.fas'   -delete
	@find . -name '*.lib'   -delete
	@find . -name '*.abcl'  -delete
	@find . -name '*.fsl'   -delete
	@find . -name '*.o'     -delete
	@echo "cleaned"
