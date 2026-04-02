# ICL 1.21.0 Release Notes

## Summary

Inline completion hints, truecolor theme support, and cross-implementation debugger fixes.

## New Features

- Add inline completion hints (ghost text). As you type, the common completion suffix appears as dim text after the cursor. Press Right or End to accept the hint. Configurable via `*inline-hints-enabled*`.
- Migrate color system from 256-color ANSI codes to truecolor hex (#RRGGBB) via tuition's complete-color objects. Themes now specify exact colors that are automatically downsampled on terminals with limited color support.
- Add per-theme paren-match background colors, parenthesis color, and text style flags (italic comments, bold special forms/keywords) to terminal themes.
- Add theme-derived markdown styles for `,explain` command output.

## Bug Fixes

- Fix debugger crash on non-interactive stdin for CCL/ECL backends.
- Disable interactive debugger for non-SBCL backends that don't support it.

## Dependencies

- Update dockview-core to 5.1.0.
- Update tuition, mgl-pax, clingon, cl-cookie, cl-selfupdate, dexador, drakma, closer-mop, named-readtables, pure-tls, and dref.

## Breaking Changes

None.
