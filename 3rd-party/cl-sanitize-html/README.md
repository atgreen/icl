# cl-sanitize-html

OWASP-style HTML sanitization library for Common Lisp, designed for safely rendering untrusted HTML content (like HTML emails or user-generated content).

## Features

- **Whitelist-based sanitization** - Only explicitly allowed tags and attributes pass through
- **Multiple security policies** - Default, Strict, and Email policies included
- **XSS prevention** - Blocks script tags, event handlers, javascript: URLs, and other attack vectors
- **CSS sanitization** - Optional CSS property filtering for email content
- **Safe defaults** - Automatically adds `rel="noopener noreferrer"` and `target="_blank"` to links
- **Plump-based** - Built on the robust Plump HTML parser
- **Well-tested** - Comprehensive test suite covering OWASP attack vectors

## Quick Start

```lisp
(use-package :sanitize-html)

;; Basic usage with default policy
(sanitize "<script>alert('XSS')</script><p>Hello</p>")
;; => "<p>Hello</p>"

;; Remove event handlers
(sanitize "<a href='#' onclick='alert(1)'>Click me</a>")
;; => "<a href=\"#\" rel=\"noopener noreferrer\" target=\"_blank\">Click me</a>"

;; Use email policy for HTML emails
(sanitize "<table><tr><td bgcolor='red'>Cell</td></tr></table>" *email-policy*)
;; => "<table><tr><td bgcolor=\"red\">Cell</td></tr></table>"
```

## Security Policies

### Default Policy (*default-policy*)

Balanced security and usability for general web content:
- **Allowed tags**: Common formatting and semantic tags (p, div, span, a, strong, em, lists, tables, etc.)
- **Allowed protocols**: http, https, mailto, ftp
- **Inline styles**: Blocked
- **Comments**: Removed

### Strict Policy (*strict-policy*)

Maximum security with minimal formatting:
- **Allowed tags**: Only basic formatting (a, b, em, strong, ul, ol, li, p, br, code, pre)
- **Allowed protocols**: https, mailto only
- **Very limited attributes**: Only href, title, and class

### Email Policy (*email-policy*)

Designed for HTML emails with legacy formatting:
- **Allowed tags**: All email-safe tags including tables, font, center
- **Allowed protocols**: http, https, mailto, cid (inline images), data (base64)
- **Inline styles**: Allowed with filtered CSS properties
- **Table attributes**: bgcolor, cellpadding, cellspacing, etc.

## API

### Main Functions

```lisp
(sanitize html-string &optional policy)
(sanitize-html html-string &optional policy)
```

Sanitize HTML string according to policy. Returns sanitized HTML string.

**Parameters:**
- `html-string` - String containing HTML to sanitize
- `policy` - Security policy to apply (defaults to `*default-policy*`)

**Returns:** Sanitized HTML string

**Example:**
```lisp
(sanitize "<script>bad</script><p>good</p>")
;; => "<p>good</p>"
```

### Utility Functions

```lisp
(safe-url-p url &optional policy)
```

Check if URL uses a safe protocol according to policy.

```lisp
(sanitize-url url &optional policy)
```

Return URL if safe, nil otherwise.

### Custom Policies

```lisp
(make-policy &key allowed-tags allowed-attributes allowed-protocols
                  allowed-css-properties remove-comments escape-cdata)
```

Create a custom security policy.

**Example:**
```lisp
(defparameter *my-policy*
  (make-policy
   :allowed-tags '("p" "br" "a" "strong" "em")
   :allowed-attributes '(("a" . ("href" "title")))
   :allowed-protocols '("https")
   :remove-comments t))

(sanitize html-string *my-policy*)
```

## Security Features

### XSS Prevention

- ✅ Script tags removed
- ✅ Event handlers (onclick, onload, etc.) removed
- ✅ javascript: protocol blocked
- ✅ data: protocol blocked (except in email policy with validation)
- ✅ Inline styles blocked (except in email policy with CSS filtering)
- ✅ Form elements blocked
- ✅ iframe/object/embed blocked
- ✅ meta/link/style/base blocked

### CSS Injection Prevention

- CSS properties filtered by whitelist (email policy only)
- `javascript:`, `expression()`, `@import` blocked in CSS values
- `behavior:` property blocked (IE-specific XSS vector)

### Safe Defaults

- Links automatically get `rel="noopener noreferrer"` (prevents tabnabbing)
- Links automatically get `target="_blank"` (open in new tab)
- Comments removed by default
- CDATA sections escaped by default

## Email HTML Example

```lisp
(defun render-email-html (email-html-body)
  "Safely render HTML email content"
  (sanitize-html email-html-body *email-policy*))

;; Typical email HTML with inline styles and tables
(render-email-html "
  <table border='1' cellpadding='10'>
    <tr>
      <td bgcolor='#ff0000'>
        <p style='color: white; font-size: 16px'>
          Welcome to our newsletter!
        </p>
      </td>
    </tr>
  </table>
  <p>
    <a href='https://example.com'>Visit our site</a>
  </p>
")
```

## Running Tests

```lisp
(asdf:test-system :sanitize-html)
```

Or manually:

```lisp
(asdf:load-system :sanitize-html/tests)
(fiveam:run! :sanitize-html-tests)
```

## Dependencies

- **plump** - Lenient HTML/XML parser
- **lquery** - DOM manipulation
- **cl-ppcre** - Regular expressions for CSS parsing
- **alexandria** - Utilities library

**Test dependencies:**
- **fiveam** - Unit testing framework

## Architecture

1. **Parser** - Uses Plump to parse HTML into a DOM tree
2. **Tree Walker** - Recursively visits each node in the DOM
3. **Policy Enforcer** - Checks each element/attribute against whitelist
4. **Sanitizer** - Removes or modifies unsafe content
5. **Serializer** - Converts sanitized DOM back to HTML string

## Comparison with Other Libraries

| Feature | sanitize-html | bluemonday (Go) | ammonia (Rust) | bleach (Python) |
|---------|------------------|-----------------|----------------|-----------------|
| Whitelist-based | ✅ | ✅ | ✅ | ✅ |
| Multiple policies | ✅ | ✅ | ✅ | ❌ |
| CSS sanitization | ✅ | ✅ | ✅ | ✅ |
| URL validation | ✅ | ✅ | ✅ | ✅ |
| Link safety | ✅ | ✅ | ❌ | ❌ |
| OWASP-aligned | ✅ | ✅ | ✅ | ✅ |

## References

- [OWASP XSS Prevention Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Cross_Site_Scripting_Prevention_Cheat_Sheet.html)
- [HTML5 Security Cheat Sheet](https://html5sec.org/)
- [Plump Documentation](https://shinmera.github.io/plump/)

## Author and License

``sanitize-html`` was written by [Anthony Green](https://github.com/atgreen)
and is distributed under the terms of the MIT license.
