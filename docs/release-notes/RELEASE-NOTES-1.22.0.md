# ICL 1.22.0 Release Notes

## Summary

Undo/redo support, theme tab-completion, and terminal compatibility fixes.

## New Features

- Add undo/redo with Ctrl-Z / Ctrl-Y. Undo stack stores up to 100 snapshots. New edits clear the redo stack.
- Add tab-completion for `,theme` subcommands (`list`, `terminal`, `browser`, `auto`) and theme names.
- Remap suspend from Ctrl-Z to Ctrl-\ to make room for undo.

## Bug Fixes

- Fix OSC 11 background query to use ST (ESC \) terminator instead of BEL for broader terminal compatibility (Ghostty, etc.).
- Increase OSC 11 query timeout from 50ms to 100ms for slower terminals.
- Drain stale terminal input (e.g. late OSC 11 responses) before starting the editor to prevent garbage at the prompt.
- Fix browser theme auto-selection to fall back to current terminal theme when the client doesn't report a dark mode preference, instead of re-querying the terminal.
- Use 500-column width for browser terminal to enable horizontal scrolling instead of wrapping.

## Improvements

- Rework dark mode detection to use OSC 11 luminance query directly instead of a hardcoded terminal name list. Default to dark mode when detection fails.
- Change `*terminal-dark-mode-override*` default from NIL to :auto.

## Breaking Changes

- Ctrl-Z is now undo (was suspend). Use Ctrl-\ to suspend.
