# ICL 1.25.0 Release Notes

## Summary

Feature release focused on notebooks.  Tabular data now renders as an
interactive, pivotable grid (or a chart) powered by a bundled, offline
Apache Arrow + Perspective engine; notebooks gain interactive widgets,
headless parameterized execution, slide export, and a raft of authoring
conveniences.

## New Features

### Interactive data tables and charts

- A list of plists/alists, or a Lisp-Stat `data-frame`, renders as a
  sortable, filterable, **pivotable** grid.  Drag columns into *Group By*
  to pivot and aggregate, or switch the same data to a bar, line, scatter,
  **heatmap**, **treemap**, or sunburst chart from the viewer's settings —
  no chart spec required.
- Powered by typed Apache **Arrow** and **Perspective**, both bundled, so
  it works fully offline.  Column types are preserved (numbers stay
  numeric), the grid layout is saved with the notebook, and any output can
  be maximized to fill the window (`Esc` restores).

### Interactive widgets

- Cells can create controls bound to a symbol — `(slider 'n :min 0 :max
  100)`, `(dropdown 'r choices)`, `(checkbox 'b)`, `(text-input 's)`,
  `(button 'go)`.  Changing a control re-runs the cells that reference its
  symbol, reactively, with no callbacks to wire up.

### Headless, parameterized execution

- Run a notebook without a browser and save the result:
  `icl --notebook report.iclnb --execute --param region=West -o out.iclnb`.
  `--param NAME=VALUE` (repeatable) overrides bindings before the run, so
  parameters declared with `defvar` pick up the override.

### Authoring and export

- **Per-cell execution time**, shown in each cell header.
- **Run above / Run below** and **Restart & run below** toolbar actions,
  and a per-cell **collapse-input** toggle (show only output).
- **Cell tags** (`parameters`, `hide-input`, `slide`, …).
- **Find & replace** across all cells (`Ctrl-F`).
- A **variable inspector** listing everything the notebook has defined.
- **`(out N)`** to reuse the value of an earlier cell by its run number.
- Markdown **admonitions** (`> [!NOTE]`, `[!WARNING]`, `[!TIP]`, …) and
  blockquotes.
- **Download buttons** on rich outputs — a grid exports to CSV, a chart or
  diagram to SVG.
- Export to a **reveal.js slide deck** (driven by slide tags, output-first),
  in addition to the existing HTML and `.lisp` exports.  Grids and charts
  are snapshotted so they render in the exported files.

## Bug Fixes

- Notebook tables are no longer capped at 5000 rows; large tables stream
  through Arrow.
- Grid outputs size to their content instead of a fixed height.
- Fixed the offset of the click-to-inspect symbol highlight in notebook
  cell editors.

## Breaking Changes

None.
