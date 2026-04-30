# ICL 1.23.6 Release Notes

## Summary

Fixes input handling for terminals using the Kitty keyboard protocol
and corrects escape tracking in paredit string detection.

## Bug Fixes

- Fixed characters like `"` being silently dropped in terminals that
  use the Kitty keyboard protocol (kitty, foot, ghostty, WezTerm,
  etc.).  ICL now properly decodes printable characters, modifier
  combinations, and special keys sent as `ESC[N;Mu` sequences.
- Fixed the Shift+Enter modifier check for the Kitty protocol, which
  was testing the wrong bit due to the protocol's `1 + flags`
  encoding.
- Fixed `in-string-p` and `count-unmatched-delimiters` where escaped
  characters inside strings were not being skipped correctly, causing
  incorrect string state tracking in paredit mode.

## Breaking Changes

None.
