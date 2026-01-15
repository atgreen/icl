# ICL 1.20.0 Release Notes

## Summary

This release adds self-update functionality, allowing ICL to update itself directly from GitHub releases.

## New Features

- **Self-update command**: New `icl update` CLI command to check for and install updates from GitHub releases.
  - `icl update` - Download and install the latest version
  - `icl update --check` / `-c` - Check if an update is available without installing
  - `icl update --dry-run` / `-n` - Download the update but don't install it

- **Version in help**: The ICL version is now displayed in the `icl --help` output.

## Infrastructure

- **Linux tarballs**: CI now produces standalone Linux tarballs (`icl-VERSION-linux-amd64.tar.gz`) for self-update support.

- **macOS artifact naming**: macOS release artifacts renamed from `macos` to `darwin` to match platform detection conventions (`darwin-arm64`, `darwin-amd64`).

## Dependencies

- Added `cl-selfupdate` library for GitHub release downloads
- Using `pure-tls` for TLS support without requiring OpenSSL

## Breaking Changes

None.
