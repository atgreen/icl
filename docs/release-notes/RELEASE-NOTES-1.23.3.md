# ICL 1.23.3 Release Notes

## Summary

GPG-signed DEB packages, APT repository, and clean version strings in all release builds.

## New Features

- DEB packages are now GPG-signed.
- New APT repository hosted via GitHub Pages for Debian/Ubuntu users:
  ```bash
  curl -fsSL https://atgreen.github.io/icl/deb-repo/icl-archive-keyring.gpg | sudo tee /usr/share/keyrings/icl-archive-keyring.asc > /dev/null
  echo "deb [signed-by=/usr/share/keyrings/icl-archive-keyring.asc] https://atgreen.github.io/icl/deb-repo stable main" | sudo tee /etc/apt/sources.list.d/icl.list
  sudo apt update
  sudo apt install icl
  ```

## Bug Fixes

- Removed `.git` directory before building in all CI jobs (RPM, DEB,
  Linux tarball, Windows, macOS) to prevent `+dirty` suffixes in
  version strings.
- Added verification steps that fail the build if a dirty version is
  detected.

## Breaking Changes

None.
