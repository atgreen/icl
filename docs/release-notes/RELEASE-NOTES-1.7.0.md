# ICL 1.7.0 Release Notes

## New Features

### HTTP MCP Server for AI CLI Integration
- Added Hunchentoot-based HTTP MCP server for live REPL access
- AI CLIs (Claude, Gemini, Codex) can now access live session state via MCP tools
- MCP server starts on-demand during `,explain` command (not at REPL startup)
- Server binds to localhost only (127.0.0.1) for security

### New MCP Tools
- `get_repl_history` - Get recent REPL interactions (inputs and results)
- `list_project_files` - List files in current working directory
- `read_project_file` - Read project files (with path traversal protection)
- `get_session_info` - Get current session info (package, recent values, etc.)

### AI CLI Support
- **Claude**: HTTP transport with `--mcp-config` and `--allowedTools`
- **Gemini**: Streamable HTTP transport with `httpUrl`
- **Codex**: Inline `--config` with `--enable rmcp_client`

### New Commands
- `,edit` (`,ed`) - Open source file for a symbol in `$EDITOR`

## Improvements

### Command Improvements
- `,info` now shows MCP server status
- `,trace` and `,untrace` provide better error messages when given forms instead of symbol names

### REPL History
- REPL interactions are now tracked for MCP tool access
- History includes input, result, and error status

## Bug Fixes

- Fixed HTTP MCP server returning empty JSON for notifications (now returns HTTP 202 Accepted)

## Dependencies

- Added `hunchentoot` for HTTP MCP server

## Breaking Changes

None.
