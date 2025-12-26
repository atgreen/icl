# ICL 1.15.2 Release Notes

## New Features

### Function Disassembly Visualization
The `,viz` command now supports functions, displaying their disassembly in a dedicated panel:

```lisp
,viz #'mapcar
```

This opens a themed panel showing the function's machine code disassembly. The display automatically adapts to light/dark mode.

## Bug Fixes

- Fixed refresh handling for function visualizations in browser mode
