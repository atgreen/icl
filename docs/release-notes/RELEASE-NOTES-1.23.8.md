# ICL 1.23.8 Release Notes

## Summary

Restores package changes (`in-package`, `,cd`) at the REPL, adds
pass-through of arguments after `--` to the inferior Lisp, and rolls up
the fixes previously prepared for 1.23.7.

## New Features

- Arguments following `--` on the ICL command line are now passed
  through to the inferior Lisp, allowing scripts and options to be
  forwarded to the backend.

## Bug Fixes

- Fixed `in-package`, `(setf *package* ...)`, and the `,cd` command
  having no lasting effect at the REPL.  The interactive debugger eval
  path bound `*package*` to `COMMON-LISP-USER` around both the read and
  the evaluation of each form, so any package change was discarded
  before the next form was read.
- Fixed REPL crash (package-lock-violation on COMMON-LISP) when
  typing `"` inside a form with a dotted package name like
  `zr.strings:parse-f64`.  The pathname context checker was using
  `intern` on the `:cl` package instead of `find-symbol`, which
  triggered SBCL's package lock when the extracted function name
  wasn't a known CL symbol.
- Hardened `form-complete-p` to catch all errors from
  `read-from-string`, not just `reader-error`.  This prevents
  package-lock violations and other unexpected conditions from
  crashing the editor on Enter.
- Fixed escaped character tracking in `find-string-start` where
  `(incf i)` inside `(loop for i ...)` had no effect.
- `ros install atgreen/icl` now fails with a clear, actionable message
  when run without a prior `ocicl install`, instead of an obscure
  error.

## Breaking Changes

None.
