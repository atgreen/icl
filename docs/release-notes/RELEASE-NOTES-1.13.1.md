# ICL 1.13.1 Release Notes

## Bug Fixes

### Output Formatting
- Fix spurious `%` character appearing after `,ql` / `,load-system` commands. The `~~%%` pattern in nested format strings was incorrectly producing a literal `%` after newlines.
- Same fix applied to MCP server documentation output, profiler folded stack output, and trace command output.

## Other Changes

### Test Suite
- Add comprehensive tests for completion functions (word char classification, path detection, prefix matching)
- Add tests for syntax highlighting (tokenizer, paren matching, display width)
- Add tests for indentation (whitespace handling, indent calculation, reindentation)
- Add `make check` as alias for `make test`
- Test suite now has 204 checks

## Breaking Changes

None.
