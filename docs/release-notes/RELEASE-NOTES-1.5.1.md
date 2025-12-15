# ICL 1.5.1 Release Notes

## New Features

### Multi-Implementation Support
- ABCL (Armed Bear Common Lisp) now works as a backend
- ECL (Embeddable Common Lisp) now works as a backend
- Configure via `~/.iclrc`: see README for details

### Environment Variables
- Added `ICL_ASDF_PATH` environment variable for packagers to specify bundled ASDF location

## Bug Fixes

- Fixed cross-implementation Slynk protocol compatibility (strings were being serialized in SBCL-specific format)
- Fixed shutdown sequence that was sending SBCL-specific quit command to other implementations

## Documentation

- Updated installation section with Windows binary formats
- ABCL and ECL marked as tested in supported implementations table

## Breaking Changes

None.
