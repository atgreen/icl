# ICL 1.8.0 Release Notes

## New Features

### Cross-Reference (Xref) Commands
Sly-style cross-reference commands for navigating code:
- `,callers <symbol>` (`,xc`) - Show functions that call a symbol
- `,callees <symbol>` (`,xe`) - Show functions called by a symbol
- `,references <symbol>` (`,xr`) - Show code that references a variable

## Bug Fixes

### Keyboard
- Fixed Ctrl-B and Ctrl-F keybindings (GitHub #5) - Ctrl-B now correctly moves cursor backward

### Configuration
- `*prompt-string*` is now honored by the prompt generator
- `*continuation-prompt*` can now override the default continuation prompt

### Documentation Lookup
- Package-qualified symbols now work with `,doc` (e.g., `,doc cl:car`)

### Path Handling
- Fixed `~` expansion to handle edge cases (`"~"` alone, `"~user"` syntax)
- Hardened MCP path validation to reject Windows drive paths (`C:\...`)
- MCP glob patterns now validated to prevent directory traversal attacks

## Documentation

- Added architecture diagram to README showing ICL, inferior Lisp, and AI CLI integration
- Documented new xref commands

## Breaking Changes

None.
