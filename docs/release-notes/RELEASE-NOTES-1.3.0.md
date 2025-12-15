# ICL 1.3.0 Release Notes

## New Features

### Profiling (SBCL only)
- `,profile <form>` - Profile a form with the statistical profiler
- `,profile-start` / `,ps` - Start ongoing profiling
- `,profile-stop` / `,pst` - Stop profiling and show results
- `,profile-reset` - Reset profiler data

### Debugging
- `,step <form>` - Show traced function calls with call tree, arguments, and return values

### Tab Completion
- Context-aware completion: packages for `,cd`, files for `,load`, systems for `,ql`
- Tab completion for command names

### Development Commands
- `,load-system` / `,ql` - Load systems via ocicl, Quicklisp, or ASDF
- `,time <form>` - Time expression evaluation
- `,load <file>` - Load a Lisp file
- `,compile-file <file>` - Compile a file
- `,disassemble <fn>` - Disassemble a function

### Configuration
- `,reload-config` / `,rc` - Reload ~/.iclrc
- `,show-config` - Show config file location and customization options

### Editor
- Ctrl-R reverse history search

## Breaking Changes

None.
