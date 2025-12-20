# ICL 1.9.1 Release Notes

## Bug Fixes

### Backend Loading
- Try ASDF/ocicl first for loading Slynk, then fall back to Quicklisp. This supports users with ocicl-configured init files.

### Browser Inspector
- Include package name in symbol info for proper inspector qualification
- Handle inspection errors gracefully with user-visible error messages
- Fix race condition where inspection data could arrive before panel initialization
- Inspector now shows error details instead of silently failing

### Cursor Positioning
- Fix cursor positioning for lines that wrap beyond terminal width (fixes #9)
- Ctrl-A now correctly moves to the beginning of wrapped lines

### Text Selection
- Fix text selection in browser REPL - click-and-drag now works for copying text (fixes #10)

## Breaking Changes

None.
