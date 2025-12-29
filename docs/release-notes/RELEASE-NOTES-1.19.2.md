# ICL 1.19.2 Release Notes

## Bug Fixes

- **Fix `,doc` command**: The `,doc` command was failing with "Package ICL does not exist" error. Local variables in forms sent to the inferior Lisp were being interned in the ICL package which doesn't exist there. Now uses `cl-user::` prefix for these variables.

## Testing

- Improved CI test for `,doc` command to actually verify documentation output instead of silently passing on timeout.
