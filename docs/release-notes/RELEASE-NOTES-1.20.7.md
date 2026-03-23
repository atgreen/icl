# ICL 1.20.7 Release Notes

## Summary

Fix premature submission of incomplete forms when input contains unknown package symbols.

## Bug Fixes

- Fix REPL submitting incomplete forms when a reader error (e.g. unknown package prefix) occurs. Previously, any reader error in `form-complete-p` / `input-complete-p` was treated as a complete form, causing unbalanced expressions like `(pkg:symbol ()` to be submitted immediately on Enter instead of allowing multiline continuation. Now checks parenthesis balance on reader errors so incomplete forms correctly continue to the next line.

## Breaking Changes

None.
