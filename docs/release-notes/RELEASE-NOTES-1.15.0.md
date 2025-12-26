# ICL 1.15.0 Release Notes

## New Features

### About Dialog
- New "About ICL" menu item in browser hamburger menu
- Shows ICL version and connected Lisp implementation (e.g., "SBCL 2.5.10")
- Author information with GitHub and LinkedIn links
- Link to Open Source Notices for all third-party licenses

### Check for Updates
- New "Check for Updates" menu item queries GitHub releases
- Compares current version with latest release
- Shows update availability with link to release page
- Includes note for package manager users (apt, dnf, choco, brew)

### Browser UI Polish
- **Connection indicator**: Green dot shows connection status, turns red on disconnect
- **Dynamic tab title**: Browser tab shows "ICL - SBCL 2.5.10" (or connected Lisp)
- **Empty state messages**: Helpful prompts in panels ("Select a package to see symbols")
- **Favicon support**: Custom ICL favicons for browser tabs and bookmarks

### Startup Banner Enhancement
- Banner now includes author credit and GitHub URL
- Format: `icl 1.15.0 (SBCL 2.5.10) [paredit]`
- `by Anthony Green - https://github.com/atgreen/icl`

## Improvements

### Self-Contained Distribution
- All JavaScript libraries now embedded (highlight.js, Vega, Vega-Lite, Vega-Embed, Mermaid)
- No external CDN dependencies - works fully offline
- Combined Open Source Notices file for all third-party licenses

### CI/CD
- Updated all installers (RPM, DEB, NSIS, MSI) with improved license handling
- Streamlined release workflow
