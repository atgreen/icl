# ICL 1.17.0 Release Notes

## New Features

### Emacs Integration
- **Multi-connection support**: Each SLY/SLIME connection can now have its own ICL browser instance. New commands `M-x icl-list` to show running instances and `M-x icl-stop-all` to stop all instances.
- **Emacs-style keybindings**: Added Ctrl+N/P for line movement, Ctrl+T for transpose characters, Ctrl+O for open line, Alt+D for kill word forward, and Alt+Backspace for kill word backward.
- **Reorganized Emacs files**: Moved `icl.el` and `icl-autoloads.el` to `emacs/` directory for cleaner project structure.

### Profiling
- **Wall-clock profiling**: New `,flame-time` (`,ftime`, `,ft`) command for profiling I/O-bound code. Samples based on elapsed time including I/O wait, ideal for network or disk operations.
- **Allocation profiling**: New `,flame-alloc` (`,falloc`, `,fa`) command for finding memory allocation hotspots.
- **Increased sample limit**: Profiler now collects up to 1 million samples (was ~50k), allowing ~16 minutes of profiling at default sample rate.

## Bug Fixes

- **CSP iframe blocking**: Fixed Content Security Policy blocking flame graph panel iframe embedding.
- **False "ICL already running" error**: Fixed issue where Emacs would report ICL as already running after the process had died.

## Breaking Changes

None.
