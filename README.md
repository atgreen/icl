# ICL - Interactive Common Lisp

ICL is an enhanced REPL for Common Lisp. It provides a modern, terminal-based interactive experience with readline-style editing, persistent history, tab completion, and an extensible command system.

## Features

- **Multi-line input** - Automatically detects incomplete expressions
- **Persistent history** - Command history saved across sessions
- **Tab completion** - Complete symbols, package-qualified names, and keywords
- **Command system** - Built-in commands prefixed with comma (e.g., `,help`)
- **Multiple Lisp support** - Works with SBCL, CCL, ECL, CLISP, ABCL, and Clasp
- **Documentation lookup** - Quick access to function docs and apropos search
- **Object inspection** - Inspect objects and view slot values
- **Tracing** - Enable/disable function tracing
- **Source location** - Find where functions are defined

## Installation

### From Package Manager

RPM-based systems (Fedora, RHEL):
```sh
dnf install icl
```

Debian-based systems:
```sh
apt install icl
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

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ICL_SLYNK_PATH` | Override path to Slynk directory |

## Architecture

ICL operates as a frontend that communicates with a backend Lisp process via the Slynk protocol (from SLY). This architecture allows ICL to:

- Work with any Common Lisp implementation
- Provide consistent features regardless of backend
- Connect to remote Lisp processes

## License

MIT License. See LICENSE file for details.

## Author

Anthony Green <green@moxielogic.com>
