# ICL 1.19.3 Release Notes

## Bug Fixes

### Coverage Reports (`,cover-report`)
- Fix coverage report generation for code using custom reader macros. Now parses sb-cover HTML output directly instead of relying on the Lisp reader.
- Fix SB-C package lock violation during coverage report generation.
- Fix coverage count accessor (use `ok-of` instead of `covered-of`).
- Improve error handling for coverage extraction failures.

## Improvements

### Coverage Report UX
- Add loading spinner while generating coverage reports. The panel opens immediately with a progress indicator instead of blocking.
- Suppress sb-cover warnings during report generation for cleaner output.
- Load file details on-demand (two-stage loading) for faster initial display.
- Focus Monaco editor automatically when switching files in coverage view.
- Add vertical indent guides to Monaco editor in coverage view.
- Remove debug output from REPL for cleaner experience.

## Breaking Changes

None.
