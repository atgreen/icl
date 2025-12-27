# ICL 1.17.1 Release Notes

## Bug Fixes

### Flame Graph Profiler
- Fix time ordering in flame graphs. Samples now display in correct chronological order (earliest on the left, latest on the right) in Speedscope's "Time Order" view.
- Fix stack frame ordering comments to correctly document that `map-trace-pc-locs` walks outer-to-inner.
- Filter flame graph traces to only include the profiler thread, excluding Slynk server threads.

## Breaking Changes

None.
