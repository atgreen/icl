# Project Review – ICL

Date: 2025-12-23

## Findings (ordered by severity)

1) MCP tool inputs can trigger reader-eval in the backend (security)
- `src/mcp-server.lisp:138-239` uses `(read-from-string ...)` on MCP-provided symbol names inside backend-eval code without disabling `*read-eval*`. A malicious client could supply `#.(...)` or other reader macros to execute arbitrary code in the backend, despite tools being “read-only”.
- Recommendation: wrap reads with `(let ((*read-eval* nil)) ...)` and parse symbols explicitly (e.g., `intern` + `find-package`), or use a safe reader that rejects `#.` and other dispatch macros.

2) Session tokens use non-cryptographic randomness (predictable tokens)
- `src/mcp-server.lisp:594-598` and `src/browser.lisp:3927-3929` call `generate-session-token`, which uses `(random 256)` and the default random state. This is predictable and weak for token security.
- Recommendation: use OS-backed randomness (`ironclad:random-data`, `sb-uuid`, `/dev/urandom`, or `uiop:with-temporary-file` + `random` seed from entropy) to generate tokens.

3) Path safety can be bypassed via symlinks
- `src/mcp-server.lisp:266-277` and `src/mcp-server.lisp:347-362` only guard against `..` and absolute paths, but do not verify the resolved path stays within the intended root. A symlink inside `ocicl/` or the project can point outside and allow reading arbitrary files.
- Recommendation: resolve `truename` and verify it is a subpath of the allowed root (also resolve the root). Reject if not a prefix match.

4) Port availability checks are SBCL-only, leading to collisions on other hosts
- `src/backend.lisp:301-324` returns `nil` on non-SBCL platforms, so `find-free-port` will return the first candidate even if it’s in use. `start-browser` uses the same `port-in-use-p` check (`src/browser.lisp:3919-3925`).
- Impact: on non-SBCL builds, auto-port selection can collide, causing startup failures or binding to unexpected ports.
- Recommendation: implement a portable socket bind check (usocket) or retry with error handling if the bind/listen fails.

5) MCP file reads load full files before truncation (memory risk)
- `src/mcp-server.lisp:354-358` reads the entire file with `uiop:read-file-string` and only then truncates at 50k chars.
- Recommendation: read with a fixed-size buffer or check file size first (`file-length` on a stream) and stream up to the limit.

6) CLI `--connect` parsing can crash on invalid ports
- `src/main.lisp:95-101` uses `parse-integer` without handling errors. Input like `--connect localhost:abc` will throw and exit ungracefully.
- Recommendation: wrap `parse-integer` in `handler-case` and return a user-facing error.

7) MCP stdio assumes line-delimited JSON only
- `src/mcp-server.lisp:552-563` uses `read-line` to parse JSON-RPC. If a client sends pretty-printed JSON or messages containing newlines, parsing fails.
- Recommendation: implement length-delimited framing (per MCP stdio guidance) or read until a complete JSON object is parsed.

8) Missing test coverage
- No test harness or test directory is present. The codebase has complex REPL, backend, and protocol logic that would benefit from basic unit/functional tests (parser, backend lifecycle, MCP tools).

## Suggested Improvements & Optimizations

- Harden MCP input handling
  - Add size limits for incoming HTTP bodies and stdio messages; reject over a safe threshold before parsing.
  - Enforce “initialize” handshake before allowing tool calls (currently `*mcp-session-initialized*` is only used to prevent double init).

- Improve portability
  - Replace SBCL-only process and port checks with `uiop:run-program` + `usocket` or a small portability layer; degrade gracefully on non-SBCL hosts.

- Safer file reads
  - For `read_project_file`/`read_source_file`, use `truename` prefix checks and read bounded chunks. Consider binary-safe reads or detect and refuse binary files.

- Browser launch robustness
  - `src/browser.lisp:3952-3954` assumes `xdg-open`; consider platform-specific openers or a no-op if not available.

- Logging & observability
  - Add log rotation or size limits for `~/.icl-mcp.log` to avoid unbounded growth.

## Quick Wins

- Wrap all MCP `read-from-string` usage with `*read-eval*` disabled and restrict to symbol parsing.
- Add a small `safe-truename-under-root` utility and use it in MCP file tools.
- Add minimal tests for MCP tools and `parse-connect-string` error handling.

