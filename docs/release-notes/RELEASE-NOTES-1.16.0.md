# ICL 1.16.0 Release Notes

## New Features

### Cached SBCL Image for Faster Startup

ICL now caches a pre-built SBCL image with Slynk already loaded, dramatically reducing startup time after the first run:

- **Automatic Caching**: On first startup, ICL creates `~/.cache/icl/icl-<version>-sbcl-<version>-slynk.core`
- **Version-Aware**: Cache automatically invalidates when ICL or SBCL version changes
- **`--no-cache` Option**: New CLI flag to disable cached image creation/usage

### Inline Tree Expansion in Browser Inspector

The browser inspector now supports Clouseau-style inline expansion:

- **Expandable Entries**: Click the arrow next to inspectable values to expand them inline
- **Nested Depth Colors**: Nested expansions use color-coded borders (yellow, orange, purple, blue, green)
- **Cached Children**: Expanded children are cached for instant collapse/expand toggling

### GitHub Pages Landing Page

New landing page at the project site with:

- Animated floating parentheses background
- Interactive terminal preview
- Auto-completing Lisp implementation showcase

## Bug Fixes

- Fixed inspector CAR/CDR buttons incorrectly showing as enabled for non-cons objects
- Keep REPL focus when opening visualization panels

## Breaking Changes

None.
