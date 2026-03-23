# ICL 1.20.8 Release Notes

## Summary

Add Ctrl-C interrupt support for the browser REPL and fix premature submission of incomplete forms.

## Bug Fixes

- Fix REPL submitting incomplete forms when a reader error (e.g. unknown package prefix) occurs. Previously, any reader error was treated as a complete form, causing unbalanced expressions to be submitted immediately on Enter instead of allowing multiline continuation. Now checks parenthesis balance on reader errors.

## New Features

- Add Ctrl-C support in the browser REPL to interrupt running evaluations. Pressing Ctrl-C while code is executing sends an `:emacs-interrupt` message via the Slynk protocol to break into the running computation on the backend.

## Breaking Changes

None.
