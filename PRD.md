# ICL - Interactive Common Lisp

## Product Requirements Document

### Executive Summary

ICL (Interactive Common Lisp) is an enhanced REPL for Common Lisp, inspired by Ruby's IRB. While Common Lisp implementations ship with basic REPLs, and tools like SLIME/SLY provide IDE-level features within Emacs, there is no standalone, terminal-based enhanced REPL that provides a modern, feature-rich interactive experience for Common Lisp developers who prefer working outside of Emacs or want a lightweight alternative.

### Problem Statement

Current Common Lisp REPL options have limitations:

1. **Implementation REPLs** (SBCL, CCL, etc.) - Minimal features, no syntax highlighting, basic history
2. **SLIME/SLY** - Excellent but tied to Emacs; not usable standalone
3. **SLIMA** - Atom-based, discontinued
4. **No terminal-native solution** exists with modern REPL features like auto-completion, syntax highlighting, and integrated documentation

### Vision

ICL will be the definitive standalone REPL for Common Lisp - beautiful, powerful, and implementation-agnostic. It should feel as natural to Common Lisp developers as IRB feels to Ruby developers.

### Target Users

1. **Primary**: Common Lisp developers who work in terminals or non-Emacs editors
2. **Secondary**: Developers learning Common Lisp who want an approachable REPL
3. **Tertiary**: Emacs users who need a quick REPL without starting their full IDE

---

## Core Features

### Phase 1: Foundation

#### 1.1 Enhanced REPL Loop
- **Multi-line input**: Automatically detect incomplete expressions and continue reading
- **Parenthesis matching**: Visual indication of matching parens during input
- **Input validation**: Validate syntax before evaluation, provide meaningful error messages
- **Graceful error handling**: Catch errors, display backtraces, allow recovery without REPL crash

#### 1.2 Syntax Highlighting
- Real-time syntax highlighting during input
- Highlight: special forms, macros, functions, variables, strings, numbers, comments
- Customizable color schemes
- Support for 256-color and true-color terminals

#### 1.3 Command History
- Persistent history across sessions (stored in `$XDG_STATE_HOME/icl/history`, default `~/.local/state/icl/history`)
- Configurable history size (default: 1000 entries)
- History search: `Ctrl-R` for reverse incremental search
- History command: `(history)` or `,history` to view/search history
- Multi-line expressions stored as single history entries

#### 1.4 Tab Completion
- Complete symbols (functions, variables, macros, special forms)
- Complete package-qualified symbols (`pkg:sym<TAB>`)
- Complete pathnames in string contexts
- Complete slot names for CLOS objects
- Complete keyword arguments
- Show completion candidates in a menu

---

### Phase 2: Developer Tools

#### 2.1 Documentation Integration
- **show-doc**: Display documentation for symbols
  ```
  icl> ,doc mapcar
  ```
- **apropos**: Search for symbols by partial name
  ```
  icl> ,apropos "string"
  ```
- Integration with CLHS (Common Lisp HyperSpec) for standard symbols
- Support for docstrings from loaded systems

#### 2.2 Source Code Browsing
- **show-source**: Display source code for functions/macros
  ```
  icl> ,source defun
  icl> ,source my-function
  ```
- Show file location and line numbers
- Syntax-highlighted source display
- Open source in `$EDITOR` with `,edit` command

#### 2.3 Inspection & Object Browsing
- **inspect**: Interactive object inspector
  ```
  icl> ,inspect *my-object*
  ```
- Navigate object structure (slots, elements, etc.)
- CLOS object inspection (class, slots, methods)
- Hash-table, array, and struct inspection

#### 2.4 Workspace Navigation
- **cd**: Change current package context
  ```
  icl> ,cd :my-package
  ```
- **pwd**: Show current package
- **ls**: List symbols in current package (with filters)
  ```
  icl> ,ls              ; all exported symbols
  icl> ,ls :internal    ; internal symbols
  icl> ,ls :functions   ; only functions
  icl> ,ls :classes     ; only classes
  ```

---

### Phase 3: Debugging

#### 3.1 Breakpoint Support
- `(break)` function integration - drop into ICL at breakpoints
- Show source context at breakpoint location
- Inspect local variables in breakpoint context
- Continue/abort from breakpoints

#### 3.2 Debugger Integration
- Integrate with implementation debugger (SBCL, CCL, etc.)
- Backtrace display with syntax highlighting
- Frame inspection and navigation
- Restart selection
- Step/next/continue commands

#### 3.3 Tracing & Profiling
- **trace/untrace**: Trace function calls
  ```
  icl> ,trace my-function
  icl> ,untrace my-function
  ```
- **time**: Time expression evaluation (wraps `time`)
- **profile**: Basic profiling support
  ```
  icl> ,profile (my-expensive-operation)
  ```

---

### Phase 4: Advanced Features

#### 4.1 ASDF Integration
- Load systems: `,load-system :system-name`
- Quickload: `,ql :system-name` (if Quicklisp available)
- Show loaded systems: `,systems`
- Reload changed files: `,reload`

#### 4.2 Configuration System
- Configuration file: `~/.iclrc` (Common Lisp code)
- Customizable prompt
- Custom commands (user-defined)
- Color scheme customization
- Key binding customization

Configuration example:
```lisp
;; ~/.iclrc
(icl:set-option :prompt "λ> ")
(icl:set-option :history-size 5000)
(icl:set-option :color-scheme :solarized-dark)

;; Custom command
(icl:define-command :reload-project ()
  "Reload the current project"
  (asdf:load-system :my-project :force t))
```

#### 4.3 Evaluation History
- `*` - last result
- `**` - second-to-last result
- `***` - third-to-last result
- `+`, `++`, `+++` - last input expressions
- `/`, `//`, `///` - last returned values (all values)
- Access by index: `(icl:result 5)` for result of input #5

#### 4.4 Command System
Commands prefixed with `,` for non-Lisp operations:
```
,help              - Show all commands
,quit / ,exit      - Exit ICL
,doc <symbol>      - Show documentation
,source <symbol>   - Show source code
,edit <symbol>     - Edit source in $EDITOR
,inspect <expr>    - Inspect object
,trace <symbol>    - Trace function
,untrace <symbol>  - Untrace function
,cd <package>      - Change package
,pwd               - Show current package
,ls [options]      - List symbols
,history [n]       - Show history
,load <file>       - Load Lisp file
,compile <file>    - Compile file
,ql <system>       - Quickload system
,apropos <string>  - Search symbols
,describe <symbol> - Describe symbol
,macroexpand <form> - Expand macro
,disassemble <fn>  - Disassemble function
```

#### 4.5 Pager Support
- Automatically page long output
- Configurable pager (default: `less` or built-in)
- Disable with `,set pager off`

---

### Phase 5: Polish & Ecosystem

#### 5.1 Implementation Support
- **Primary**: SBCL (most popular, best introspection)
- **Secondary**: CCL (Clozure Common Lisp)
- **Tertiary**: ECL, ABCL, Clasp
- Abstract implementation-specific features behind clean interface

#### 5.2 Terminal Support
- Support modern terminal emulators
- Graceful degradation for limited terminals (TERM=dumb)
- Mouse support (optional) for completion menus
- Clipboard integration (`,copy` command)

#### 5.3 Input Methods
- **Reline-style**: Full multiline editing with history (default)
- **Readline**: Traditional line editing
- **Simple**: Basic input for non-interactive use

#### 5.4 Extensibility API
- Plugin system for third-party extensions
- Hook system for customization:
  - `*before-eval-hook*`
  - `*after-eval-hook*`
  - `*prompt-hook*`
  - `*error-hook*`
- Public API for:
  - Defining custom commands
  - Custom completers
  - Custom inspectors
  - Output formatters

---

## Technical Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────┐
│                        ICL REPL                         │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │   Input     │  │   Output    │  │    Commands     │  │
│  │   Handler   │  │   Handler   │  │    Registry     │  │
│  └──────┬──────┘  └──────┬──────┘  └────────┬────────┘  │
│         │                │                   │          │
│  ┌──────┴──────┐  ┌──────┴──────┐  ┌────────┴────────┐  │
│  │  Readline/  │  │   Pretty    │  │    Command      │  │
│  │  Reline     │  │   Printer   │  │    Parser       │  │
│  │  Backend    │  │   + Colors  │  │                 │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │  Completer  │  │   History   │  │   Inspector     │  │
│  │             │  │   Manager   │  │                 │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │   Syntax    │  │  Source     │  │   Debugger      │  │
│  │   Highlighter│  │  Finder    │  │   Interface     │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────┤
│            Implementation Abstraction Layer             │
├─────────────────────────────────────────────────────────┤
│    SBCL    │    CCL     │    ECL    │    ABCL    │ ... │
└─────────────────────────────────────────────────────────┘
```

### Key Dependencies (from LispIndex via ocicl)

#### Core - Line Editing & Input

| Library | Purpose | Notes |
|---------|---------|-------|
| **linedit** | Readline-style line editing | Completions, history, paren matching, multi-line. Works with SBCL/CCL. Fully customizable in CL. |
| **cl-readline** | GNU Readline bindings | Emacs/vi modes, history, completion. GPL 3. |
| **clinenoise** | Linenoise bindings | Lightweight readline alternative |

**Recommendation**: Start with **linedit** (pure CL, customizable) or evaluate **tuition** for a more modern approach.

#### Core - Terminal UI & Output

| Library | Purpose | Notes |
|---------|---------|-------|
| **cl-ansi-text** | Terminal colors | 8-color, 24-bit RGB, HSV, hex colors |
| **cl-ansi-term** | Terminal primitives | Colors, tables, progress bars, style sheets |
| **terminfo** | Terminal capabilities | Query terminal features, adapt behavior |
| **termp** | Terminal detection | Detect dumb vs real terminals |
| **tuition** | Modern TUI framework | TEA architecture, rich styling, mouse support, borders, layouts. Bubble Tea-inspired. |

**Recommendation**: **cl-ansi-text** for colors, **termp** for detection. Consider **tuition** for advanced UI.

#### Core - Introspection & MOP

| Library | Purpose | Notes |
|---------|---------|-------|
| **closer-mop** | MOP compatibility | Essential for portable MOP code. Supports SBCL, CCL, ECL, ABCL, etc. |
| **moptilities** | MOP utilities | Slot introspection, function arguments |
| **trivial-arguments** | Lambda-list inspection | Get function signatures portably |
| **graven-image** | Enhanced REPL utilities | Improved apropos*, describe*, inspect*, time*, benchmark* |

**Recommendation**: **closer-mop** + **moptilities** + **graven-image** for introspection.

#### Core - Debugging & Stack Traces

| Library | Purpose | Notes |
|---------|---------|-------|
| **dissect** | Stack trace capture | Pretty-printing, portable |
| **trivial-backtrace** | Simple backtrace API | Portable, lightweight |
| **trivial-custom-debugger** | Debugger override | Full control over debugger, works with break |
| **ndebug** | Custom debugger toolkit | Interface-aware debugger hooks |

**Recommendation**: **dissect** + **trivial-custom-debugger** for debugger integration.

#### Development Tools

| Library | Purpose | Notes |
|---------|---------|-------|
| **repl-utilities** | REPL helpers | Common interactive development utilities |
| **conium** | Swank components | Source path parsing, file caching. IDE integration. |
| **swank** (from slime) | SLIME backend | Source location, inspection. Optional heavyweight dep. |
| **printv** | Debug tracing | Print expressions with values, thread-safe |

**Recommendation**: **repl-utilities** for utilities, optionally **conium** for source location.

#### CLI & Configuration

| Library | Purpose | Notes |
|---------|---------|-------|
| **clingon** | CLI argument parsing | Subcommands, shell completions, rich option types |
| **adopt** | CLI with auto help | Simple API, automatic help generation |
| **trivial-features** | Platform detection | Consistent *features* across implementations |

**Recommendation**: **clingon** for CLI, **trivial-features** for portability.

#### Supporting Libraries

| Library | Purpose | Notes |
|---------|---------|-------|
| **bordeaux-threads** | Threading | For async operations |
| **cl-ppcre** | Regex | For syntax highlighting patterns |
| **alexandria** | Utilities | Common utility functions |
| **cl-ascii-table** | Table display | For formatted output |

### Dependency Summary

**Minimum viable (Phase 1):**
```lisp
;; icl.asd
(defsystem "icl"
  :depends-on ("linedit"              ; Line editing
               "cl-ansi-text"         ; Colors
               "termp"                ; Terminal detection
               "closer-mop"           ; MOP portability
               "trivial-features"     ; Platform features
               "alexandria"))         ; Utilities
```

**Full featured (Phase 2+):**
```lisp
(defsystem "icl"
  :depends-on ("linedit"
               "cl-ansi-text"
               "termp"
               "closer-mop"
               "moptilities"
               "graven-image"
               "dissect"
               "trivial-custom-debugger"
               "trivial-arguments"
               "repl-utilities"
               "clingon"
               "trivial-features"
               "bordeaux-threads"
               "cl-ppcre"
               "alexandria"
               "cl-ascii-table"))
```

### File Structure

```
icl/
├── icl.asd                 # System definition
├── src/
│   ├── packages.lisp       # Package definitions
│   ├── conditions.lisp     # Error conditions
│   ├── config.lisp         # Configuration system
│   ├── repl.lisp           # Main REPL loop
│   ├── input/
│   │   ├── reader.lisp     # Input reading
│   │   ├── history.lisp    # History management
│   │   └── completion.lisp # Tab completion
│   ├── output/
│   │   ├── printer.lisp    # Pretty printing
│   │   ├── colors.lisp     # Syntax highlighting
│   │   └── pager.lisp      # Output paging
│   ├── commands/
│   │   ├── base.lisp       # Command infrastructure
│   │   ├── help.lisp       # Help commands
│   │   ├── navigation.lisp # Package navigation
│   │   ├── inspection.lisp # Object inspection
│   │   ├── source.lisp     # Source browsing
│   │   └── debug.lisp      # Debugging commands
│   ├── backends/
│   │   ├── sbcl.lisp       # SBCL-specific
│   │   ├── ccl.lisp        # CCL-specific
│   │   └── generic.lisp    # Fallback
│   └── ext/
│       └── api.lisp        # Extension API
├── contrib/                # Optional extensions
├── themes/                 # Color schemes
├── test/                   # Test suite
└── doc/                    # Documentation
```

---

## User Experience

### Default Prompt

```
CL-USER> (+ 1 2)
=> 3
CL-USER> (defun greet (name)
..       "Greet someone"
..       (format t "Hello, ~a!~%" name))
=> GREET
CL-USER> (greet "World")
Hello, World!
=> NIL
CL-USER>
```

### With Syntax Highlighting (simulated)

```
CL-USER> (defun factorial (n)
           (if (<= n 1)
               1
               (* n (factorial (1- n)))))
```
- `defun`, `if` - magenta (special forms)
- `factorial`, `<=`, `*`, `1-` - blue (functions)
- `n` - white (variables)
- `1` - cyan (numbers)

### Error Display

```
CL-USER> (/ 1 0)
ERROR: Division by zero

Backtrace:
  0: (SB-KERNEL::INTEGER-/-INTEGER 1 0)
  1: (ICL::EVAL-IN-CONTEXT (/ 1 0))
  2: (ICL::REPL-ITERATION)

Restarts:
  0: [ABORT] Return to ICL REPL

icl:debug>
```

### Completion Menu

```
CL-USER> (map<TAB>
┌──────────────────────┐
│ mapc                 │
│ mapcan               │
│ mapcar        [1/7]  │
│ mapcon               │
│ maphash              │
│ mapl                 │
│ maplist              │
└──────────────────────┘
```

---

## Success Metrics

1. **Adoption**: 500+ GitHub stars within first year
2. **Stability**: No crashes during normal operation
3. **Performance**: <50ms startup time, <10ms response for basic operations
4. **Compatibility**: Works on all major CL implementations
5. **Completeness**: 90%+ feature parity with IRB

---

## Non-Goals (Phase 1)

- IDE integration (that's SLIME/SLY's domain)
- Remote REPL (like SLIME's swank) - though local web browser UI is supported
- Standalone GUI application (we use web browser for GUI features)
- Notebook interface (like Jupyter)

**Note:** While Phase 1 originally excluded web-based features, the CLOG browser integration (`,browse`) and Xterm.js REPL consoles were added as they complement rather than replace the TUI. The primary interface remains the terminal.

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Terminal library compatibility | Abstract terminal ops, test on major terminals |
| Implementation differences | Strong abstraction layer, extensive testing |
| Performance overhead | Profile early, optimize hot paths |
| Feature creep | Strict phase discipline, MVP focus |
| Community adoption | Good documentation, compatibility with existing tools |

---

## Multi-Session REPL Architecture

### Overview

ICL supports multiple concurrent REPL sessions:
1. **TUI Session**: The primary terminal-based REPL (always exists)
2. **Browser REPL Sessions**: Multiple Xterm.js consoles in the web browser

All sessions communicate with a single backend Lisp process via Slynk. This creates challenges around output routing, stream handling, and history management.

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           ICL Main Process                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐               │
│  │  TUI Session │    │ Browser REPL │    │ Browser REPL │    ...        │
│  │              │    │   Session 1  │    │   Session 2  │               │
│  │ stdin/stdout │    │  Xterm.js    │    │  Xterm.js    │               │
│  │   (local)    │    │  WebSocket   │    │  WebSocket   │               │
│  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘               │
│         │                   │                   │                        │
│         │    ┌──────────────┴───────────────────┘                        │
│         │    │                                                           │
│  ┌──────┴────┴──────────────────────────────────────────┐               │
│  │              Session Router / Dispatcher              │               │
│  │                                                       │               │
│  │  • Tracks active session                              │               │
│  │  • Routes evaluation requests                         │               │
│  │  • Manages output destination                         │               │
│  └───────────────────────┬───────────────────────────────┘               │
│                          │                                               │
├──────────────────────────┼───────────────────────────────────────────────┤
│                          │                                               │
│  ┌───────────────────────┴───────────────────────────────┐               │
│  │                   Slynk Connection                     │               │
│  │              (Single connection to backend)            │               │
│  └───────────────────────┬───────────────────────────────┘               │
│                          │                                               │
└──────────────────────────┼───────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      Inferior Lisp Process                               │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────────┐│
│  │                        Slynk Server                                  ││
│  │                                                                      ││
│  │  • Receives eval requests                                            ││
│  │  • Executes in dynamic context                                       ││
│  │  • Returns result values (as strings)                                ││
│  └─────────────────────────────────────────────────────────────────────┘│
│                                                                          │
│  stdout/stderr → piped to ICL via async reader thread (NOT per-eval)    │
│  Note: Output is NOT captured per-eval; it flows async to active stream │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Slynk Connection Model

**Design Decision: Single Shared Connection**

All REPL sessions share a single Slynk connection to the backend. This is intentional:

1. **Single Lisp Environment**: The backend is one Lisp process with one global state. Users expect `defvar` in one session to be visible in another. Multiple connections would require multiple backend processes, defeating the purpose of a shared environment.

2. **Shared State**: Variables like `*`, `**`, `***`, packages, and the symbol table are global. Each session operates on the same Lisp image.

3. **Resource Efficiency**: Slynk connections have overhead (threads, protocol handling). One connection is sufficient since Slynk serializes requests anyway.

**How Multiple Sessions Work Over One Connection (Target Design):**

```
Session A: (defvar *foo* 42)     ──┐
                                   │    ┌─────────────────┐
Session B: (print *foo*)        ───┼───►│ Single Slynk    │───► Backend
                                   │    │ Connection      │
Session A: (setf *foo* 99)      ──┘    └─────────────────┘
```

Target flow:
1. Each session's eval request sent sequentially through the connection
2. Request includes a session ID for response routing *(not yet implemented)*
3. Backend executes in a shared environment (changes visible to all)
4. Response routed back to requesting session *(not yet implemented)*
5. Async output goes to active session or broadcasts *(currently: global active stream)*

**Current behavior (v1.8):** All sessions share the connection, but output routing uses a single global `*active-repl-output*`. No session tagging.

**Alternative: Per-Session Connections (NOT recommended)**

Could launch separate backend processes per session:
- **Pros**: Complete isolation, no state pollution
- **Cons**: No shared state (defeats purpose), heavy resource usage, can't inspect what another session defined

**Current Implementation (as of v1.8):**

```lisp
;; Single shared Slynk connection (slynk-client.lisp)
(defvar *slynk-connection* nil)

;; Simple output routing via global active stream (specials.lisp)
(defvar *active-repl-output* nil)  ; Single global, not per-session

;; Async output hook routes to whatever is "active"
(setf slynk-client:*write-string-hook* #'write-slynk-string-to-active-repl)

(defun write-slynk-string-to-active-repl (string)
  (let ((out (active-repl-output-stream)))  ; Returns *active-repl-output*
    (write-string string (or out *standard-output*))))
```

**Limitation:** Currently there's no session registry. Output goes to whichever stream was most recently set as "active". This works for single-session use but won't correctly route output when multiple sessions are evaluating concurrently.

**Target Enhancement: Session-Tagged Evaluation**

To properly route output to the requesting session:

```lisp
(defun slynk-eval-for-session (string session-id)
  "Evaluate STRING, routing all output to SESSION-ID's streams."
  (let ((*active-session-for-eval* session-id))
    (slynk-eval-form string)))
```

### Output Routing Strategy (Target Design)

**Note:** This section describes the target architecture for multi-session support. Current implementation (v1.8) uses simple global active-stream routing.

#### Categories of Output

| Output Type | Source | Target Routing |
|-------------|--------|----------------|
| **Evaluation Results** | `slynk-eval` return values | → Requesting session |
| **Captured stdout** | Code that prints during eval | → Requesting session |
| **Async stdout** | Background threads, callbacks | → Active session OR broadcast |
| **Error messages** | Conditions, warnings | → Requesting session |
| **Debug output** | `*trace-output*`, profiler | → Active session |
| **System messages** | ICL internal messages | → TUI session only |

**Current behavior (v1.8):** All output goes to `*active-repl-output*` (single global). No session-aware routing yet.

#### Proposed Implementation

1. **Session Registry** (not yet implemented)
   ```lisp
   (defvar *session-registry* (make-hash-table)
     "Map of session-id -> session object")

   (defvar *active-session* nil
     "Currently focused session (receives async output)")

   (defstruct session
     id
     type           ; :tui or :browser
     output-stream  ; Where to send output
     input-stream   ; Where to read input
     history        ; Per-session history list
     created-at)
   ```

2. **Evaluation Protocol** (target)
   - Each eval request includes the requesting session ID
   - Backend wraps eval to capture stdout/stderr
   - Response includes: `(values captured-output result-strings)`
   - Router delivers response to correct session

3. **Async Output Handling** (target)
   - Backend stdout is read by a dedicated thread in ICL
   - This output is routed to `*active-session*`
   - Optionally: broadcast mode sends to all sessions
   - TUI can be configured to always receive async output

### Stream Binding Model

#### Per-Session Stream Bindings

Each session runs in its own thread with local stream bindings:

```lisp
;; TUI Session (main thread)
(let ((*standard-input* (tui-input-stream))
      (*standard-output* (tui-output-stream))
      (*error-output* (tui-output-stream))
      (*trace-output* (tui-output-stream))
      (*debug-io* (tui-io-stream))
      (*query-io* (tui-io-stream)))
  (tui-repl-loop))

;; Browser Session (per-session thread)
(let ((*standard-input* (xterm-input-stream session))
      (*standard-output* (xterm-output-stream session))
      (*error-output* (xterm-output-stream session))
      (*trace-output* (xterm-output-stream session))
      (*debug-io* (xterm-io-stream session))
      (*query-io* (xterm-io-stream session)))
  (browser-repl-loop))
```

#### Backend Stream Handling

The inferior Lisp process has its own streams. Options:

1. **Capture Mode** (recommended): Wrap all evals to capture output
   ```lisp
   (with-output-to-string (*standard-output*)
     (eval form))
   ```

2. **Redirect Mode**: Redirect backend streams to ICL
   - Complex, requires stream proxying over Slynk

3. **Pass-through Mode**: Let backend output go to its stdout
   - ICL reads this and routes to active session

### History Management

#### Option A: Per-Session History (Recommended)

Each session maintains its own history:

```lisp
(defstruct session
  ...
  (history (make-array 1000 :adjustable t :fill-pointer 0))
  (history-file nil))  ; Optional persistence

;; TUI history: $XDG_STATE_HOME/icl/history (default ~/.local/state/icl/history)
;; Browser sessions: transient (not persisted)
```

**Pros:**
- Clear separation
- Session-specific recall
- No confusion about what you typed where

**Cons:**
- Can't recall commands from other sessions

#### Option B: Shared History with Session Tags

Single history with session metadata:

```lisp
(defstruct history-entry
  input
  timestamp
  session-id
  session-type)  ; :tui or :browser
```

**Pros:**
- Can search across all sessions
- Nothing lost

**Cons:**
- `M-p` might recall commands from other sessions

#### Option C: Hybrid (Recommended)

- **Per-session working history**: Recent commands in current session
- **Global archive**: All commands saved with tags
- **Cross-session search**: `,history --all` searches everything
- **TUI history persisted**: `$XDG_STATE_HOME/icl/history`
- **Browser history transient**: Lost on browser close (or optionally saved)

### Session Lifecycle

```
┌─────────────────────────────────────────────────────────┐
│                    Session States                        │
└─────────────────────────────────────────────────────────┘

  ┌──────────┐      focus       ┌──────────┐
  │ CREATED  │ ───────────────► │  ACTIVE  │
  └──────────┘                  └──────────┘
       │                             │
       │                             │ blur/switch
       │                             ▼
       │                        ┌──────────┐
       │                        │ INACTIVE │ ◄───┐
       │                        └──────────┘     │
       │                             │           │
       │                             │ focus     │ blur
       │                             ▼           │
       │                        ┌──────────┐     │
       │                        │  ACTIVE  │ ────┘
       │                        └──────────┘
       │                             │
       │         close               │ close
       ▼                             ▼
  ┌──────────┐                  ┌──────────┐
  │  CLOSED  │                  │  CLOSED  │
  └──────────┘                  └──────────┘
```

**Events:**
- `session-created`: New session registered
- `session-activated`: Session gains focus (becomes active)
- `session-deactivated`: Session loses focus
- `session-closed`: Session terminated, cleanup resources

### Configuration Options

```lisp
;; ~/.iclrc

;; Output routing
(setf *async-output-mode* :active-session)  ; or :broadcast, :tui-only

;; History
(setf *history-mode* :per-session)  ; or :shared, :hybrid
(setf *persist-browser-history* nil)  ; Don't save browser REPL history

;; Session defaults
(setf *browser-repl-package* "CL-USER")
(setf *sync-package-across-sessions* t)  ; Package changes sync to all
```

### User Experience Guidelines

1. **Visual Distinction**: Browser REPLs should clearly show they're separate
   - Different prompt color or prefix
   - Session ID in title bar

2. **Focus Indication**: Make it clear which session is "active"
   - Highlight active window
   - Show in status bar

3. **Output Attribution**: When output appears, indicate source if ambiguous
   - `[async]` prefix for background output
   - Session ID for cross-session messages

4. **Graceful Degradation**: If routing fails, output goes to TUI (never lost)

### Implementation Phases

**Phase 0: Current State (v1.8)**
- Single global `*active-repl-output*` for output routing
- No session registry
- Output goes to whichever stream is currently "active"

**Phase 1: Basic Isolation** (Planned)
- [ ] Per-session streams
- [ ] Active session tracking
- [ ] Evaluation results to requesting session

**Phase 2: Proper Routing** (Planned)
- [ ] Session registry
- [ ] Tagged async output
- [ ] Configurable routing modes

**Phase 3: History Separation** (Planned)
- [ ] Per-session history
- [ ] Cross-session search
- [ ] Persistent browser history option

**Phase 4: Polish** (Planned)
- [ ] Visual session indicators
- [ ] Session management commands (`,sessions`, `,switch`)
- [ ] Output attribution

---

### Phase 6: Advanced Inspector (Clouseau-Inspired)

Inspired by McCLIM's Clouseau inspector and uLisp's Structure Editor, this phase transforms the inspector from a passive viewer to an active structural editor.

#### 6.1 Zipper-Style Navigation

Enable "walking" through data structures using a zipper model:

| Key | Action | Description |
|-----|--------|-------------|
| `a` / `h` | CAR | Descend into first element of cons |
| `d` / `l` | CDR | Move to rest of list (next sibling) |
| `u` / `b` | UP | Return to parent cell |
| `Enter` | Drill | Enter selected slot/element |

**Breadcrumb Trail**: Display path from root to current focus:
```
*my-data* → [2] → :config → :options → [0]
```

#### 6.2 Focused "Keyhole" View

Instead of dumping entire structures, show only the current context:

- **Current node** displayed prominently
- **Immediate children** shown as expandable items
- **Parent context** shown as breadcrumbs (not full content)
- **Navigation stack** maintained for back/forward traversal

```
┌─ Inspector: *complex-data* ─────────────────────┐
│ Path: [root] → :config → :database             │
├─────────────────────────────────────────────────┤
│ PLIST of 4 entries                              │
│   :host      "localhost"                        │
│ ► :port      5432                    [expand]   │
│   :user      "admin"                            │
│   :options   {HASH-TABLE 3 entries}  [expand]   │
├─────────────────────────────────────────────────┤
│ [u]p [a]car [d]cdr [Enter]drill [e]dit [q]uit  │
└─────────────────────────────────────────────────┘
```

#### 6.3 In-Place Mutation

Allow editing values directly from the inspector:

| Key | Action | Description |
|-----|--------|-------------|
| `e` / `r` | Replace | Edit current value via minibuffer, `setf` back |
| `x` / `Del` | Delete | Remove element from list (modify CDR) |
| `c` / `i` | Insert | Insert new element before current (cons) |
| `C` / `I` | Append | Insert new element after current |

**Replace workflow:**
```
Inspector> [e] on slot :port
Edit value (currently 5432): 5433
=> Updated :port to 5433
```

**List mutation:**
```lisp
;; Original: (a b c d)
;; Cursor on 'c', press 'x' (delete)
;; Result: (a b d)

;; Cursor on 'b', press 'i' (insert), type "new"
;; Result: (a new b d)
```

#### 6.4 Type-Specific Presentations

Render different types with specialized views:

| Type | Presentation |
|------|--------------|
| **Cons/List** | Expandable tree OR cons-cell graph diagram |
| **Alist** | Key-value table with `→` arrows |
| **Plist** | Alternating key/value rows |
| **Hash-table** | Sortable key-value table |
| **Vector/Array** | Indexed grid with dimensions |
| **CLOS Instance** | Class name, slots grouped by inheritance |
| **Function** | Lambda list, source location, disassembly toggle |
| **Number** | Decimal, hex, binary, float components |
| **String** | With length, encoding, escape toggle |
| **Pathname** | Components: host, device, directory, name, type |
| **Condition** | Formatted report, slot values |

#### 6.5 Cons Cell Graph Visualization (Browser)

In browser mode, visualize lists as actual cons cell diagrams:

```
,viz '((a . b) c (d e))

     ┌───┬───┐    ┌───┬───┐    ┌───┬───┐
     │ ● │ ●─┼───►│ ● │ ●─┼───►│ ● │ / │
     └─┼─┴───┘    └─┼─┴───┘    └─┼─┴───┘
       │            │            │
       ▼            ▼            ▼
     ┌───┬───┐      C        ┌───┬───┐
     │ A │ B │               │ ● │ ●─┼───► ...
     └───┴───┘               └─┼─┴───┘
                               ▼
                               D
```

- **Click nodes** to inspect that element
- **Highlight** current position in zipper traversal
- **Show dotted pairs** vs proper lists distinctly

#### 6.6 Function Inspection

Enhanced function inspection capabilities:

```
┌─ Inspector: #'MAPCAR ──────────────────────────┐
│ COMPILED-FUNCTION                              │
│                                                │
│ Lambda List: (FN LIST &REST MORE-LISTS)        │
│ Type:        COMPILED-FUNCTION                 │
│ Defined in:  SB-IMPL (built-in)                │
│                                                │
│ Actions:                                       │
│   [s] Show source    [d] Disassemble          │
│   [t] Trace          [u] Untrace              │
│   [c] Show callers   [e] Show callees         │
└────────────────────────────────────────────────┘
```

For user-defined functions with source:
- **View/edit source** with syntax highlighting
- **Walk AST** using zipper navigation on the code tree
- **Modify and recompile** from within inspector

#### 6.7 Visit History & Navigation

Maintain a stack of visited objects:

| Key | Action |
|-----|--------|
| `Alt-Left` / `B` | Go back to previous object |
| `Alt-Right` / `F` | Go forward in history |
| `H` | Show visit history list |
| `g` | Go to object by history index |

```
Visit History:
  0: *my-data* (HASH-TABLE)
  1: *my-data*[:config] (PLIST)
  2: *my-data*[:config][:db] (CONNECTION)  ← current
```

#### 6.8 Evaluate in Context

Run expressions with the inspected object bound:

```
Inspector> ,eval (length *)    ; * = current object
=> 42

Inspector> ,eval (setf (gethash :new *) "value")
=> "value"
```

The inspected object is bound to `*` in the evaluation context.

#### 6.9 Implementation Notes

**Terminal Inspector Enhancements:**
- Extend existing `inspector.lisp` with zipper state
- Add mutation commands with confirmation prompts
- Use `trivial-gray-streams` for custom inspector I/O

**Browser Inspector Enhancements:**
- Add cons-cell graph component using Cytoscape.js or D3
- Implement collapsible tree view for nested structures
- WebSocket messages for mutation operations

**Dependencies:**
- Consider `cl-zipper` or implement simple zipper for cons traversal
- Use `closer-mop` for slot access and modification
- Leverage existing Slynk inspection protocol where possible

---

## Open Questions

1. Should ICL support its own package-inferred system, or require ASDF?
2. Should we reuse Swank/Slynk for source location, or implement from scratch?
3. What's the minimum supported terminal capability?
4. Should we support Windows natively or only through WSL/MSYS2?
5. How to handle implementation-specific debugger integration?
6. Should browser REPL sessions share package state with TUI?
7. How to handle debugger invocation from browser REPL (interactive restarts)?
8. Should there be a "primary" session that always receives certain output?

---

## References

- [Ruby IRB](https://github.com/ruby/irb) - Primary inspiration
- [SLIME](https://github.com/slime/slime) - Emacs CL environment
- [SLY](https://github.com/joaotavora/sly) - Modern SLIME fork
- [Pry](https://github.com/pry/pry) - Ruby alternative REPL
- [CLHS](http://www.lispworks.com/documentation/HyperSpec/Front/) - Common Lisp HyperSpec
