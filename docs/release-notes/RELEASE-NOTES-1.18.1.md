# ICL 1.18.1 Release Notes

## New Features

### Regex Railroad Diagram Visualization
New `:regexp` visualization type renders regex patterns as railroad diagrams using the Regulex JavaScript library.

```lisp
(defmethod icl-runtime:visualize ((obj my-regex-wrapper))
  (list :regexp (get-pattern-string obj)))
```

See `examples/regexp.lisp` for a complete example with a wrapper class.

## Bug Fixes

### Multiple --eval Arguments Now Work Correctly
Fixed issue where multiple `--eval` arguments were not processed correctly (GitHub issue #16).

Previously, only the last `--eval` was evaluated. Now all `--eval` expressions are processed sequentially, allowing each to see side effects from previous evals:

```bash
icl --eval "(defpackage :foo)" --eval "(in-package :foo)" --eval "(defun bar () 42)"
```

## Breaking Changes

None.
