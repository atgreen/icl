# ICL 1.23.5 Release Notes

## Summary

Fixes for DEB and RPM repository deployment and package signing.

## Bug Fixes

- Fixed DEB package build failure caused by missing `dpkg-sig` package.
- Fixed APT repository deployment to GitHub Pages so that
  `apt` package index and GPG keyring URLs are now available.
- Fixed RPM and DEB repository builds to run as part of the release
  workflow instead of as separate workflows.
- SPDX SBOM is now installed at `/usr/share/sbom/` in both RPM and DEB
  packages.

## Breaking Changes

None.
