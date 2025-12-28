# ICL 1.17.2 Release Notes

## Bug Fixes

### CCL Backend
- Fix CCL (Clozure Common Lisp) backend hanging on Slynk connection. Worker threads would hang due to `*default-worker-thread-bindings*` sharing IO streams across threads, which CCL's threading model doesn't support.

### Startup and Connection
- Suppress Slynk startup output noise ("Slynk started at port" message) for cleaner startup experience.
- Increase Slynk verification timeout for slower backends.
- Add Slynk connection verification with timeout to ensure reliable connections.
- Fix startup timing to use actual elapsed time instead of tick counts.

### Image Caching
- Include .sbclrc hash in cached image filename, ensuring cache invalidation when user modifies their init file.

## Breaking Changes

None.
