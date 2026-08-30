# ICL 1.25.1 Release Notes

## Summary

Bug-fix release.  The completion dropdown is no longer clipped in
notebook cells, and the bundled Common Lisp dependencies are refreshed.

## Bug Fixes

- Notebook code cells now grow to fit an open completion menu.  Cells
  were sized to their text only, so the completion dropdown was clipped
  at the cell boundary.  Cells now account for the menu's visible
  candidate rows (plus the `[n/total]` overflow line) when reporting
  their height to the browser.

## Dependencies

- Refresh the vendored ocicl dependency pins, adding the numeric and
  data-frame stack used by notebook tables: alexandria+, anaphora,
  array-operations, cephes, chronicity, cl-ansi-text, cl-change-case,
  cl-colors2, cl-interpol, cl-json, and others.

## Breaking Changes

None.
