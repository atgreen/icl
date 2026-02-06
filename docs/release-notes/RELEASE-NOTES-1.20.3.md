# ICL 1.20.3 Release Notes

## Summary

Bugfix release improving Monaco editor asset embedding robustness.

## Bug Fixes

### Monaco Editor Asset Embedding
- **Recursive asset discovery**: Switched from hardcoded file list to recursive directory traversal for Monaco editor assets
  - Automatically discovers all Monaco files including vite-generated hashed JavaScript and CSS files
  - Eliminates manual maintenance of asset file lists
  - Ensures Monaco editor works correctly even when build tools generate different hashed filenames

### Miscellaneous
- Updated `.gitignore` to exclude RCS and `.bak` files

## Breaking Changes

None.
