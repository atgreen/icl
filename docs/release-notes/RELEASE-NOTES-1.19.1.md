# ICL 1.19.1 Release Notes

## Bug Fixes

- **Fix Monaco Source Viewer in packaged builds**: Embed Monaco editor assets (VS library, fonts) in the binary so the source viewer works in RPM/DEB packages where filesystem assets aren't available.

- **Add WebSocket keepalive**: Send ping frames every 30 seconds to prevent browser REPL connections from timing out after periods of inactivity.

- **Improve error messages**: Better messaging when source files are unavailable or Monaco fails to load.
