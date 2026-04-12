# Release Process for icl

This document describes how to create a new release of icl with binary, RPM, DEB, and Windows packages.

## Prerequisites

- Push access to the icl repository
- Git configured with your credentials

## Release Steps

### 1. Update Version Numbers

Update the version in the following files:

- `icl.asd` - Update the `:version` field
- `README.md` - Update version references if needed
- `icl.el` - Keep `Version:` header in sync with ICL release version
- `icl.spec` - Version will be automatically updated by CI
- `debian/changelog` - Version will be automatically updated by CI

### 2. Create Release Notes

Create a new release notes file in `docs/release-notes/` named `RELEASE-NOTES-X.Y.Z.md` documenting:
- Summary of changes
- Bug fixes
- New features
- Breaking changes (if any)

See existing files in `docs/release-notes/` for format examples.

### 3. Commit Changes

```bash
git add icl.asd README.md docs/release-notes/RELEASE-NOTES-X.Y.Z.md
git commit -m "Boost version to X.Y.Z"
```

### 4. Create and Push Tag

```bash
git tag vX.Y.Z
git push origin main
git push origin vX.Y.Z
```

### 5. GitHub Actions Builds Packages

Once you push the tag, GitHub Actions will automatically:

1. **Build standalone binary** (`icl-linux-x86_64`)
   - Built using Homebrew SBCL on Ubuntu

2. **Build RPM package** (`icl-X.Y.Z-1.fc*.x86_64.rpm`)
   - Built from source in Fedora container
   - Uses `icl.spec` file

3. **Build DEB package** (`icl_X.Y.Z-1_amd64.deb`)
   - Built from source in Debian container
   - Uses `debian/` packaging files

4. **Build Windows packages** (`icl-X.Y.Z-windows-amd64.zip`, `icl-X.Y.Z-setup.exe`, `icl-X.Y.Z.msi`)
   - NSIS installer + WiX MSI
   - Includes Emacs integration files under `share/emacs/site-lisp/icl`

5. **Create GitHub Release**
   - Attaches all artifacts to the release
   - Release can be found at: `https://github.com/icl/icl/releases/tag/vX.Y.Z`

### 6. Verify Release

Check the GitHub Actions workflow at: `https://github.com/icl/icl/actions`

Verify the release includes:
- `icl-linux-x86_64` - Standalone binary
- `icl-X.Y.Z-1.*.x86_64.rpm` - RPM package
- `icl_X.Y.Z-1_amd64.deb` - DEB package
- `icl-X.Y.Z-windows-amd64.zip` - Windows ZIP package
- `icl-X.Y.Z-setup.exe` - Windows installer (NSIS)
- `icl-X.Y.Z.msi` - Windows installer (MSI)

### 7. Test Packages (Optional but Recommended)

#### Test RPM on Fedora:
```bash
wget https://github.com/icl/icl/releases/download/vX.Y.Z/icl-X.Y.Z-1.*.x86_64.rpm
sudo dnf install ./icl-X.Y.Z-1.*.x86_64.rpm
icl version
icl setup
```

#### Test DEB on Debian/Ubuntu:
```bash
wget https://github.com/icl/icl/releases/download/vX.Y.Z/icl_X.Y.Z-1_amd64.deb
sudo apt install ./icl_X.Y.Z-1_amd64.deb
icl version
icl setup
```

## Package Details

### RPM Package (`icl.spec`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-devel, gcc
- Installs binary to `/usr/bin/icl`
- Installs Emacs files to `/usr/share/emacs/site-lisp/icl`

### DEB Package (`debian/`)
- Builds from source using SBCL
- Dependencies: sbcl, libfixposix-dev, gcc
- Installs binary to `/usr/bin/icl`
- Installs Emacs files to `/usr/share/emacs/site-lisp/icl`

### Windows Packages (NSIS/WiX)
- ZIP, EXE (NSIS), and MSI (WiX) artifacts
- Installs under `C:\Program Files\ICL`
- Emacs files land at `share\emacs\site-lisp\icl`

## Troubleshooting

### Build fails in GitHub Actions
- Check the Actions tab for error logs
- Common issues:
  - SBCL build errors (check dependencies)
  - Version string mismatches
  - Missing files in tarball

### Package installation fails
- Verify dependencies are installed
- Check architecture (packages are x86_64 only currently)
- Ensure sufficient disk space for SBCL build

## Future Improvements

- [ ] Add ARM64 builds
- [ ] Add macOS packages
- [ ] Add Windows packages
- [ ] Automate homebrew formula updates
- [x] Sign RPM packages with GPG
- [x] Sign DEB packages with GPG
