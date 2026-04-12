# ICL 1.23.2 Release Notes

## Summary

Fix RPM signing in release pipeline.

## Bug Fixes

- Install rpm-sign package in Fedora build container so rpmsign is available.
- RPM packages are now GPG-signed with the icl signing key.

## Breaking Changes

None.
