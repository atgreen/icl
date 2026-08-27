# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Install dependencies (requires ocicl package manager)
ocicl install

# Build the icl executable
make

# Clean build artifacts
make clean
```

The build requires SBCL, [ocicl](https://github.com/ocicl/ocicl), and libfixposix-devel.

## Running

```bash
# Start ICL with auto-detected Lisp backend
./icl

# Specify a backend
./icl --lisp sbcl
./icl --lisp ccl

# Evaluate and exit
./icl -e '(+ 1 2 3)'

# Connect to existing Slynk server
./icl --connect localhost:4005
```

## Architecture Overview

ICL is a frontend REPL that communicates with a backend Lisp process via the Slynk protocol (from SLY). This client-server architecture allows it to work with multiple Lisp implementations.

### Key Components

- **src/main.lisp** - Entry point, CLI parsing (uses clingon)
- **src/repl.lisp** - Main REPL loop, coordinates input/output/eval
- **src/slynk-client.lisp** - Wraps the slynk-client library for backend communication
- **src/backend.lisp** - Backend process management, Lisp implementation detection
- **src/editor.lisp** - Multi-line input editor with readline-style editing
- **src/buffer.lisp** - Input buffer management for the editor
- **src/paredit.lisp** - Structural editing (auto-close parens, sexp navigation)
- **src/completion.lisp** - Tab completion for symbols and packages
- **src/highlight.lisp** - Syntax highlighting with terminal colors
- **src/inspector.lisp** - TUI object inspector
- **src/mcp-server.lisp** - HTTP server for AI CLI integration (read-only tools)
- **src/browser.lisp** - Web-based IDE interface (Hunchentoot + WebSockets)
- **src/commands/** - Extensible comma-prefixed command system

### Command System

Commands are defined using `define-command` in `src/commands/core.lisp`:

```lisp
(define-command (help h ?) ()
  "Show available commands."
  ...)
```

Commands can have aliases (like `h` and `?` above) and receive parsed arguments as strings.

### Terminal Abstraction

Platform-specific terminal handling:
- `src/terminal-posix.lisp` - POSIX termios-based raw mode
- `src/terminal-windows.lisp` - Windows console API

### Dependencies

Third-party code is vendored in:
- `3rd-party/slynk-client/` - Modified slynk-client for backend communication
- `ocicl/` - Dependencies managed by ocicl (including Slynk from SLY)

## Release Process

See `docs/RELEASING.md`. Key files to update:
- `icl.asd` - `:version` field
- Create `docs/release-notes/RELEASE-NOTES-X.Y.Z.md`

Tag with `vX.Y.Z` to trigger GitHub Actions build.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:46cd31e7 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/core-concepts/sync-concepts.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->

<!-- BEGIN BEADS CODEX SETUP: generated by bd setup codex -->
## Beads Issue Tracker

Use Beads (`bd`) for durable task tracking in repositories that include it. Use the `beads` skill at `.agents/skills/beads/SKILL.md` (project install) or `~/.agents/skills/beads/SKILL.md` (global install) for Beads workflow guidance, then use the `bd` CLI for issue operations.

### Quick Reference

```bash
bd ready                # Find available work
bd show <id>            # View issue details
bd update <id> --claim  # Claim work
bd close <id>           # Complete work
bd prime                # Refresh Beads context
```

### Rules

- Use `bd` for all task tracking; do not create markdown TODO lists.
- Run `bd prime` when Beads context is missing or stale. Codex 0.129.0+ can load Beads context automatically through native hooks; use `/hooks` to inspect or toggle them.
- Keep persistent project memory in Beads via `bd remember`; do not create ad hoc memory files.

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/core-concepts/sync-concepts.md for details and anti-patterns.
<!-- END BEADS CODEX SETUP -->
