# ICL 1.23.9 Release Notes

## Summary

Hardens the local browser interface against two low-severity issues and
rolls up the JavaScript dependency upgrades (dockview, mermaid,
vega-lite) prepared after 1.23.8.

## Security

- Fixed a path-traversal weakness in the browser's static asset serving.
  The `/assets/` and `/speedscope/` routes are not token-gated and passed
  the request path into the filesystem-fallback lookup without
  validation, so a crafted percent-encoded request (e.g.
  `/assets/..%2f..%2f..%2fetc%2fpasswd`) could escape the assets
  directory and read files readable by the ICL user when an on-disk
  assets directory was present.  Filenames containing `..` components or
  absolute paths are now rejected before any lookup.
- Fixed the WebSocket origin check to match the loopback host exactly.
  It previously used a substring match, so a spoofed origin such as
  `http://127.0.0.1.evil.com` was accepted.  The host is now parsed and
  compared against the loopback allow-list.

Both issues are reachable only on the loopback interface and behind the
random port and per-session token, so real-world exposure is limited.

## Dependencies

- Update dockview from 5.2.0 to 7.0.2.  In v7 upstream renamed the
  framework-agnostic core package from `dockview-core` to `dockview`,
  which changes the browser global from `window['dockview-core']` to
  `window.dockview`; all dockview APIs used by ICL are unchanged.
- Update mermaid from 11.14.0 to 11.16.0.
- Update vega-lite from 6.4.2 to 6.4.3.

## Breaking Changes

None.
