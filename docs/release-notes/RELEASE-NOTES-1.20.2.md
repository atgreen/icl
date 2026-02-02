# ICL 1.20.2 Release Notes

## Summary

Bugfix release with updated vendored libraries and dependency updates.

## Updates

### JavaScript Libraries
- **vega-lite**: Updated to 6.4.2
  - Fixed tooltip data showing "undefined" with nearest hit testing
  - Fixed tooltip checking for custom formatters
  - Fixed legend merging with explicit color scale/range/domain

### Lisp Dependencies
- **pure-tls**: Updated to 20260118
  - Added ACME certificate profile support
  - Improved certificate revocation checking across platforms
  - Better native certificate verification with custom trusted roots

- **cl-tuition**: Updated to 20260124 (v1.3.0)
  - Added datepicker component for calendar date selection
  - Added drop shadows and title bars for borders
  - Added exec-cmd for external program execution
  - Enhanced keyboard support (Page Up/Down, Insert, modified arrow keys)
  - Added set-terminal-title function

- **cl-selfupdate**: Updated to 20260120
  - Added USE_LEGACY_OPENSSL option for legacy OpenSSL environments

- **serapeum**: Updated to 20260126
  - Minor refactoring and documentation updates

## Bug Fixes

None.

## Breaking Changes

None.
