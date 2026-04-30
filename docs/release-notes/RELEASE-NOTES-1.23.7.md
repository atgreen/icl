# ICL 1.23.7 Release Notes

## Summary

Fixes a crash when typing `"` inside forms that reference
package-qualified symbols (e.g. `zr.strings:parse-f64`).

## Bug Fixes

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

## Breaking Changes

None.
