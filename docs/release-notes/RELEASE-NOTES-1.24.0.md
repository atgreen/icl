# ICL 1.24.0 Release Notes

## Summary

Feature release.  ICL gains browser-based notebooks — a live,
cell-oriented workspace backed by a real Lisp session — and the terminal
REPL learns to select and copy text with the mouse.

## New Features

### Notebooks

ICL now includes a full notebook environment that runs in the browser
alongside the package browser, inspector, and visualizations.  A notebook
is a sequence of cells you edit and evaluate against a live Lisp session.

- Start one from the terminal with the `,notebook [file]` command, or
  launch directly into a notebook with `icl --notebook [file]`.  Filename
  completion works after `,notebook`.
- Code cells use the same editor as the REPL — syntax highlighting,
  paredit, and completion.  Press Enter for a newline and Shift-Enter to
  run the cell.  A cell may contain several forms; every form is
  evaluated and the last one produces the cell's value.
- Rich output is rendered inline: values, printed output, hash tables,
  class diagrams, Vega-Lite charts, and Mermaid diagrams.  Call
  `(display x)` to emit several rich outputs from a single cell.  Large
  images fit to the cell width and can be zoomed and panned.
- Markdown cells support headings, emphasis, lists, GitHub-flavored
  tables, and KaTeX math (`$…$` and `$$…$$`).
- Command mode with keyboard shortcuts, running/queued cell indicators,
  a table of contents built from your markdown headings, collapsible and
  scrollable outputs, and per-cell controls to run, move, duplicate, and
  delete.
- Manage the session with Interrupt, Restart, and Restart & Run All.
- Notebooks autosave, show a dirty indicator, and can be exported to a
  plain `.lisp` file or a self-contained HTML page.  The on-disk format
  is a readable `.iclnb` s-expression file.

See `examples/notebook-tour.iclnb` for a guided tour.

### Terminal mouse selection and clipboard

The terminal REPL now supports selecting text with the mouse.  Text is
copied to the clipboard automatically when you release the drag, so you
can lift output straight out of the session.

## Bug Fixes

- Fix Enter at the end of the buffer so a complete top-level form is
  submitted reliably.

## Breaking Changes

None.
