# ICL 1.23.12 Release Notes

## Summary

Rolls up the JavaScript dependency upgrades for the browser interface:
dockview, Monaco Editor, Vega, and mermaid are updated to their latest
releases.

## Dependencies

- Update dockview from 7.0.2 to 8.1.0.  v8 is a major release that
  splits some new capabilities into a separately licensed
  `dockview-enterprise` package; the framework-agnostic core ICL bundles
  remains MIT-licensed and the browser global is still `window.dockview`.
  All dockview APIs used by ICL are unchanged, and the panel layout was
  verified to build and render correctly.  ICL is unaffected by the v8
  breaking changes (the `onDidDimensionsChange` content-area reporting
  change and the `moveTo*` -> `activate*` renames), neither of which ICL
  uses.
- Update Monaco Editor from 0.55.1 to 0.56.0.  The vendored `monaco/vs`
  tree is now the official `min/vs` distribution; it preserves the AMD
  loader contract ICL relies on and the editor was verified to
  initialize.
- Update Vega from 6.2.0 to 6.4.0.
- Update mermaid from 11.16.0 to 11.16.1.

## Breaking Changes

None.
