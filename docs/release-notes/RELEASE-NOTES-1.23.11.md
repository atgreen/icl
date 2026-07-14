# ICL 1.23.11 Release Notes

## Summary

Fixes `icl:configure-lisp` crashing ICL at startup when registering a
Lisp implementation not in the built-in table (issue #44).

## Bug Fixes

- Fixed `configure-lisp` for implementations not in the built-in
  table.  The add-new-entry path built the entry's property list with
  `list*`, leaving a dotted tail (`:eval-arg . "--eval"`), so the
  first `getf` on the entry signaled "malformed property list" --
  registering any Lisp outside the built-in set (e.g.
  `(icl:configure-lisp :clamiga ...)` in `~/.iclrc`) crashed ICL at
  startup.  Thanks to Manfred Bergmann for the report and diagnosis.

## Breaking Changes

None.
