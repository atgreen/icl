# ICL 1.23.10 Release Notes

## Summary

Fixes `read-line` (and other reads from `*standard-input*`) hanging
forever at the REPL, in both the terminal and the browser interface
(issue #42).

## Bug Fixes

- Fixed `(read-line)` never returning at the REPL.  Evaluations run on
  Slynk worker threads in the inferior Lisp, and those threads'
  `*standard-input*` pointed at the inferior process's stdin pipe --
  a pipe ICL never writes to -- so any read blocked forever and typed
  input was silently swallowed.  Worker-thread input is now routed
  through the `:read-string` protocol: the backend requests a line
  from ICL, which reads it from the session that is evaluating and
  sends it back.  `read`, `y-or-n-p`, and other `*query-io*` reads
  work through the same channel.
- Fixed the same hang in the browser (`-b`) terminal, which previously
  had no input path at all during evaluation.  Input typed in the
  browser terminal while the backend is reading is echoed, supports
  backspace editing, and Ctrl-D on an empty line signals end-of-file.
- Prompts printed just before a read (e.g. `(format t "Name: ")
  (force-output) (read-line)`) are now flushed and visible before ICL
  waits for input.
- Interrupting an evaluation with a pending read abandons the read
  instead of stealing the first line typed at the next prompt.

## Breaking Changes

None.
