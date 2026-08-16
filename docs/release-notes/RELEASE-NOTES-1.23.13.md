# ICL 1.23.13 Release Notes

## Summary

Bug-fix release.  ICL no longer fails to start when `$PATH` contains a
relative entry, and the bundled Common Lisp dependencies are refreshed.

## Bug Fixes

- Fix a startup crash when `$PATH` contains a non-absolute entry, such as
  an unexpanded `~/.dotnet/tools`.  The PATH search used
  `uiop:getenv-absolute-directories`, which signals an error on the first
  relative entry it encounters, preventing ICL from launching at all.
  ICL now tolerates relative entries and resolves them against the current
  directory, exactly as a POSIX shell does.  Thanks to Visen for the
  report.  (#45)

## Dependencies

- Refresh vendored ocicl dependency pins: alexandria, cffi/uffi, clingon,
  float-features, iolib, jsown, local-time, named-readtables, pure-tls,
  serapeum, slynk, termp, trivial-gray-streams, trivial-indent, and
  tuition.

## Breaking Changes

None.
