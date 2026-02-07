# ICL 1.20.3 Release Notes

## Summary

Bugfix release improving Monaco editor asset embedding robustness.

## Bug Fixes

### Browser Interface
- **Fixed package and symbol search**: Removed inline `oninput` event handlers that were being blocked by Content Security Policy
  - Package filter input now works correctly
  - Symbol filter input now works correctly
  - Eliminates CSP violation warnings in browser console

### Monaco Editor Asset Embedding
- **Recursive asset discovery**: Switched from hardcoded file list to recursive directory traversal for Monaco editor assets
  - Automatically discovers all Monaco files including vite-generated hashed JavaScript and CSS files
  - Eliminates manual maintenance of asset file lists
  - Ensures Monaco editor works correctly even when build tools generate different hashed filenames

### Miscellaneous
- Updated `.gitignore` to exclude RCS and `.bak` files

## Breaking Changes

None.
