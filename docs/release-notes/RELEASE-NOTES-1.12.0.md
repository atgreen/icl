# ICL 1.12.0 Release Notes

## Highlights

**FSet Venn diagram visualization.** Visualize FSet sets as interactive Venn diagrams showing membership and intersections.

**Auto-refreshing visualizations.** Hash-table, class hierarchy, and Venn diagram panels now automatically update after each REPL evaluation.

## New Features

### FSet Set Visualization

Visualize [FSet](https://github.com/slburson/fset) sets as Venn diagrams:

- **Single set**: Circle displaying members inside
- **Two sets**: Classic Venn diagram with intersection
- **Three sets**: Triangle arrangement showing all 7 regions

Examples:
```lisp
ICL> ,viz *fruits*                    ; Single set as circle
ICL> ,viz *fruits* *red-things*       ; Two-set Venn diagram
ICL> ,viz *set-a* *set-b* *set-c*     ; Three-set Venn diagram
```

Features:
- Custom SVG rendering (no external dependencies)
- Theme-aware colors using CSS variables
- Catppuccin-inspired colors (blue/pink/green)
- Members displayed in each region

### Auto-Refreshing Visualizations

All visualization panels now refresh automatically after REPL evaluation:

- **Hash-tables**: Re-query and display updated contents
- **Class hierarchies**: Refresh to show definition changes
- **Venn diagrams**: Re-evaluate sets to reflect modifications

### Panel Maximize/Restore

New ways to maximize panels in the browser interface:

- **Double-click tab**: Toggle maximize/restore
- **F11 key**: Toggle maximize for focused panel
- **Escape key**: Exit maximized state

### Responsive Class Hierarchy

The class hierarchy graph now resizes when its panel is resized, using ResizeObserver with debounced re-rendering.

## Improvements

### Command Rename

- Renamed `,browse` command to `,browser` for clarity

### Browser Lifecycle

- Browser view automatically closes when ICL process terminates
- WebSocket `onclose` handler detects disconnection

### Bug Fixes

- Fixed class graph node click incorrectly switching to Symbol Info tab
- Added `source` field to symbol-click messages to preserve panel focus

## Technical Details

- Venn diagrams use pure SVG (no D3 or external libraries)
- Set intersection computed client-side using JavaScript Sets
- ResizeObserver used for responsive panel content
- Panel state tracked in Maps for refresh coordination

## Breaking Changes

- The `,browse` command has been renamed to `,browser`

## Requirements

- Browser interface for visualizations (`,browser` or `icl -b`)
- FSet library for set visualization (`(ql:quickload :fset)`)
