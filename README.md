# ICL - Interactive Common Lisp

ICL is an enhanced REPL for Common Lisp. It provides a modern, terminal-based interactive experience with readline-style editing, persistent history, tab completion, and an extensible command system.

<p align="center">
  <img src="assets/demo.gif" alt="ICL Demo">
</p>

## Features

- **Syntax highlighting** - Colorized input with distinct colors for keywords, strings, comments, numbers, and symbols
- **Parenthesis matching** - Real-time highlighting of matching parentheses as you type
- **Multi-line input** - Automatically detects incomplete expressions with smart indentation
- **Persistent history** - Command history saved across sessions
- **Tab completion** - Complete symbols, package-qualified names, and keywords
- **Command system** - Built-in commands prefixed with comma (e.g., `,help`)
- **Multiple Lisp support** - Works with SBCL, CCL, ECL, CLISP, ABCL, and Clasp
- **Error backtraces** - Automatic backtrace capture with `,bt` command to view stack traces
- **Thread inspection** - List and inspect threads in the inferior Lisp
- **Documentation lookup** - Quick access to function docs and apropos search
- **Object inspection** - Inspect objects and view slot values
- **Tracing** - Enable/disable function tracing
- **Source location** - Find where functions are defined
- **Terminal-aware colors** - Automatically detects light/dark terminal background

## Installation

### Pre-built Packages

Download the latest RPM or DEB package from the [GitHub Releases](https://github.com/atgreen/icl/releases) page.

RPM-based systems (Fedora, RHEL):
```sh
sudo dnf install ./icl-*.rpm
```

Debian-based systems:
```sh
sudo dpkg -i icl_*.deb
```

### Building from Source

Requirements:
- SBCL
- ocicl
- libfixposix-devel (for osicat)

```sh
git clone https://github.com/atgreen/icl.git
cd icl
ocicl install
make
```

The resulting `icl` binary can be copied anywhere.

## Usage

Start ICL (auto-detects available Lisp):
```sh
icl
```

Specify a Lisp implementation:
```sh
icl --lisp ccl
icl --lisp ecl
```

Evaluate an expression and exit:
```sh
icl -e '(+ 1 2 3)'
```

Load a file before starting the REPL:
```sh
icl -l init.lisp
```

Connect to an existing Slynk server:
```sh
icl --connect localhost:4005
```

Skip loading ~/.iclrc:
```sh
icl --no-config
```

## Commands

Commands are prefixed with a comma. Type `,help` for a full list.

### Navigation

| Command | Description |
|---------|-------------|
| `,cd <package>` | Change current package |
| `,pwd` | Show current package |
| `,ls [filter]` | List symbols (filters: functions, macros, variables, classes) |

### Documentation

| Command | Description |
|---------|-------------|
| `,doc <symbol>` | Show documentation |
| `,describe <symbol>` | Full description of symbol |
| `,apropos <pattern>` | Search for matching symbols |
| `,arglist <function>` | Show function arguments |
| `,source <symbol>` | Show source location |

### Inspection

| Command | Description |
|---------|-------------|
| `,inspect <expr>` | Inspect an object |
| `,slots <expr>` | Show slots of a class instance |

### Macros

| Command | Description |
|---------|-------------|
| `,macroexpand <form>` | Expand macro once |
| `,macroexpand-all <form>` | Fully expand all macros |

### Debugging

| Command | Description |
|---------|-------------|
| `,bt` | Show backtrace from last error |
| `,threads` | List all threads in inferior Lisp |
| `,trace <function>` | Enable tracing |
| `,untrace <function>` | Disable tracing |
| `,untrace-all` | Disable all tracing |

### Session

| Command | Description |
|---------|-------------|
| `,help` | Show all commands |
| `,info` | Show session information |
| `,history` | Show value history variables |
| `,lisp [name]` | Show or switch Lisp backend |
| `,clear` | Clear terminal |
| `,quit` | Exit ICL |

## History Variables

ICL maintains history of recent values and inputs:

| Variable | Description |
|----------|-------------|
| `icl:_` / `icl:icl-*` | Last result |
| `icl:__` / `icl:icl-**` | Second-to-last result |
| `icl:___` / `icl:icl-***` | Third-to-last result |
| `icl:icl-+` | Last input form |
| `icl:icl-/` | Last returned values (all values) |

## Configuration

ICL loads `~/.iclrc` on startup (unless `--no-config` is specified). This file can contain any Common Lisp code.

Example `~/.iclrc`:
```lisp
;; Change default package
(in-package :cl-user)

;; Load commonly used systems
(asdf:load-system :alexandria)

;; Define custom utilities
(defun reload ()
  (asdf:load-system :my-project :force t))
```

## Keyboard Shortcuts

| Key | Description |
|-----|-------------|
| `Enter` | Submit form if complete, otherwise insert newline |
| `Alt+Enter` | Always insert newline (works in most terminals) |
| `Shift+Enter` | Always insert newline (requires kitty keyboard protocol) |
| `Tab` | Complete symbol or show completion menu |
| `Ctrl+A` / `Home` | Move to beginning of line |
| `Ctrl+E` / `End` | Move to end of line |
| `Ctrl+K` | Kill to end of line |
| `Ctrl+U` | Clear entire line |
| `Ctrl+L` | Clear screen |
| `Ctrl+D` | EOF (exit if line empty) |
| `Ctrl+C` | Cancel current input |
| `Up/Down` | Navigate history (on first/last line) or move cursor |
| `Alt+Q` | Reindent current form |

**Note:** Use Alt+Enter in gnome-terminal and most other terminals. Shift+Enter only works in terminals with kitty keyboard protocol support (kitty, WezTerm, Alacritty, etc.).

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ICL_SLYNK_PATH` | Override path to Slynk directory |
| `ICL_BACKGROUND` | Override terminal background detection (`dark` or `light`) |
| `NO_COLOR` | When set to any non-empty value, disables colored output (see [no-color.org](https://no-color.org/)) |

## Architecture

ICL operates as a frontend that communicates with a backend Lisp process via the Slynk protocol (from SLY). This architecture allows ICL to:

- Work with any Common Lisp implementation
- Provide consistent features regardless of backend
- Connect to remote Lisp processes

## License

MIT License. See LICENSE file for details.

## Author

Anthony Green <green@moxielogic.com>
