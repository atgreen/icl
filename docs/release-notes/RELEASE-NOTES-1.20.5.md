# ICL 1.20.5 Release Notes

## Summary

Bugfix release fixing a startup failure when the user's `.sbclrc` does not load ASDF.

## Bug Fixes

- Fix UIOP package error during cached image creation when `.sbclrc` doesn't load ASDF. The generated Lisp code contained a literal `uiop:getcwd` symbol that the reader couldn't resolve before ASDF was loaded. Split `(require :asdf)` into a separate `--eval` so ASDF packages exist before the image creation code is read.

## Breaking Changes

None.
