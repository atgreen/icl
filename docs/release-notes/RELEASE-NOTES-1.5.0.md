# ICL 1.5.0 Release Notes

## New Features

### Windows Support
- **Native Windows packages**: ZIP archive, NSIS installer (EXE), WiX installer (MSI)
- Windows CI testing in GitHub Actions
- Slynk path discovery works correctly on Windows

### Roswell Installation
- Added Roswell script for portable installation across platforms
- Install with `ros install atgreen/icl` (requires ocicl)

### Multi-Implementation Improvements
- **Bundled ASDF**: ICL now bundles ASDF for Lisp implementations that don't include it (e.g., CLISP)
- **Portable evaluation**: Changed to captured output for better cross-implementation compatibility
- CLISP support is now experimental (known connection stability issues)

### Verbose Mode
- Added `--verbose` / `-v` flag for startup debugging
- Shows Slynk connection details and backend status

## Bug Fixes

- Fixed ASDF package error on Windows SBCL
- Strip misleading "Stream:" context from reader error messages

## Documentation

- Added "Supported Lisp Implementations" section to README
- SBCL is the tested platform; other implementations are untested or experimental

## Breaking Changes

None.
