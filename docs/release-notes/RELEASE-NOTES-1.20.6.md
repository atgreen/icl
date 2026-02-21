# ICL 1.20.6 Release Notes

## Summary

Update bundled JavaScript libraries and fix a cached image creation bug.

## Changes

- Update dockview-core from 4.13.1 to 5.0.0. This major version bump adds header positioning support; all APIs used by ICL are unchanged.
- Update mermaid from 11.12.2 to 11.12.3 (patch release).
- Fix SLYNK package reader error during cached image creation. The generated eval code contained a literal `slynk:create-server` symbol that the reader couldn't resolve before Slynk was loaded.

## Breaking Changes

None.
