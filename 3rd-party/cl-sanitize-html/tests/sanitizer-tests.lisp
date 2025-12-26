;;; tests/sanitizer-tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green
;;;
;;; Test suite for HTML sanitization

(in-package #:sanitize-html/tests)

(in-suite sanitize-html-tests)

;;; Basic sanitization tests

(test test-empty-input
  "Test that empty or nil input is handled safely"
  (is (string= "" (sanitize nil)))
  (is (string= "" (sanitize ""))))

(test test-plain-text
  "Test that plain text passes through unchanged"
  (is (string= "Hello, world!" (sanitize "Hello, world!"))))

(test test-safe-html
  "Test that safe HTML is preserved"
  (let ((html "<p>This is <strong>safe</strong> HTML</p>"))
    (is (search "<p>" (sanitize html)))
    (is (search "<strong>" (sanitize html)))
    (is (search "safe" (sanitize html)))))

;;; XSS attack prevention

(test test-script-tag-removed
  "Test that script tags are removed"
  (let ((result (sanitize "<script>alert('XSS')</script><p>Content</p>")))
    (is (not (search "<script" result)))
    (is (not (search "alert" result)))
    (is (search "<p>" result))
    (is (search "Content" result))))

(test test-event-handlers-removed
  "Test that event handlers are removed"
  (let ((result (sanitize "<a href='#' onclick='alert(1)'>Click</a>")))
    (is (not (search "onclick" result)))
    (is (search "<a" result))
    (is (search "href" result))))

(test test-javascript-protocol
  "Test that javascript: protocol is blocked"
  (let ((result (sanitize "<a href='javascript:alert(1)'>Bad</a>")))
    (is (not (search "javascript:" result)))))

(test test-data-protocol-blocked-by-default
  "Test that data: protocol is blocked in default policy"
  (let ((result (sanitize "<img src='data:text/html,<script>alert(1)</script>'>")))
    (is (not (search "data:" result)))))

;;; Tag filtering

(test test-disallowed-tags-removed
  "Test that disallowed tags are removed but content preserved"
  (let ((result (sanitize "<iframe src='evil.com'>Content</iframe>")))
    (is (not (search "<iframe" result)))
    (is (search "Content" result))))

(test test-form-elements-removed
  "Test that form elements are removed"
  (let ((result (sanitize "<form><input type='text'></form>")))
    (is (not (search "<form" result)))
    (is (not (search "<input" result)))))

(test test-object-embed-removed
  "Test that object and embed tags are removed"
  (is (not (search "<object" (sanitize "<object data='evil.swf'></object>"))))
  (is (not (search "<embed" (sanitize "<embed src='evil.swf'></embed>")))))

;;; Attribute filtering

(test test-safe-attributes-preserved
  "Test that safe attributes are preserved"
  (let ((result (sanitize "<a href='http://example.com' title='Example'>Link</a>")))
    (is (search "href" result))
    (is (search "title" result))
    (is (search "example.com" result))))

(test test-style-attribute-removed-by-default
  "Test that style attribute is removed in default policy"
  (let ((result (sanitize "<p style='color: red'>Text</p>")))
    (is (not (search "style" result)))))

(test test-class-and-id-preserved
  "Test that class and id attributes are preserved"
  (let ((result (sanitize "<div class='container' id='main'>Content</div>")))
    (is (search "class=\"container\"" result))
    (is (search "id=\"main\"" result))))

;;; URL sanitization

(test test-http-urls-allowed
  "Test that HTTP URLs are allowed"
  (let ((result (sanitize "<a href='http://example.com'>Link</a>")))
    (is (search "http://example.com" result))))

(test test-https-urls-allowed
  "Test that HTTPS URLs are allowed"
  (let ((result (sanitize "<a href='https://example.com'>Link</a>")))
    (is (search "https://example.com" result))))

(test test-mailto-urls-allowed
  "Test that mailto: URLs are allowed"
  (let ((result (sanitize "<a href='mailto:test@example.com'>Email</a>")))
    (is (search "mailto:test@example.com" result))))

(test test-relative-urls-allowed
  "Test that relative URLs are allowed"
  (let ((result (sanitize "<a href='/page'>Link</a>")))
    (is (search "/page" result))))

;;; Comment and CDATA handling

(test test-comments-removed
  "Test that HTML comments are removed by default"
  (let ((result (sanitize "<!-- comment --><p>Content</p>")))
    (is (not (search "<!--" result)))
    (is (not (search "comment" result)))
    (is (search "<p>" result))))

;;; Safe defaults

(test test-links-get-noopener
  "Test that links get rel='noopener noreferrer' added"
  (let ((result (sanitize "<a href='http://example.com'>Link</a>")))
    (is (search "noopener" result))
    (is (search "noreferrer" result))))

(test test-links-get-target-blank
  "Test that links get target='_blank' added"
  (let ((result (sanitize "<a href='http://example.com'>Link</a>")))
    (is (search "target=\"_blank\"" result))))

;;; Policy tests

(test test-strict-policy
  "Test that strict policy is more restrictive"
  (let ((html "<div><span class='test'>Content</span></div>"))
    ;; Default policy allows div and span
    (is (search "<div>" (sanitize html *default-policy*)))
    ;; Strict policy removes div but allows span with class attribute
    (let ((result (sanitize html *strict-policy*)))
      (is (not (search "<div" result)))  ; div should be removed
      (is (search "<span" result))        ; span should remain
      (is (search "class" result)))))

(test test-email-policy-allows-tables
  "Test that email policy allows table elements"
  (let* ((html "<table><tr><td>Cell</td></tr></table>")
         (result (sanitize html *email-policy*)))
    (is (search "<table>" result))
    (is (search "<tr>" result))
    (is (search "<td>" result))))

(test test-email-policy-allows-inline-styles
  "Test that email policy allows inline styles"
  (let* ((html "<p style='color: red; font-size: 14px'>Text</p>")
         (result (sanitize html *email-policy*)))
    (is (search "style" result))
    (is (search "color" result))))

(test test-email-policy-blocks-dangerous-css
  "Test that email policy blocks dangerous CSS"
  (let* ((html "<p style='color: red; behavior: url(xss.htc)'>Text</p>")
         (result (sanitize html *email-policy*)))
    (is (not (search "behavior" result)))
    (is (search "color" result))))

(test test-email-policy-allows-cid-urls
  "Test that email policy allows cid: URLs for inline images"
  (let* ((html "<img src='cid:image001@example.com'>")
         (result (sanitize html *email-policy*)))
    (is (search "cid:" result))))

;;; Edge cases

(test test-nested-tags
  "Test deeply nested tags"
  (let* ((html "<div><p><span><strong>Text</strong></span></p></div>")
         (result (sanitize html)))
    (is (search "<div>" result))
    (is (search "<p>" result))
    (is (search "<span>" result))
    (is (search "<strong>" result))))

(test test-malformed-html
  "Test that malformed HTML doesn't crash the sanitizer"
  (finishes (sanitize "<p>Unclosed"))
  (finishes (sanitize "<div><p></div></p>"))
  (finishes (sanitize "<<script>alert(1)</script>")))

(test test-unicode-content
  "Test that Unicode content is preserved"
  (let* ((html "<p>Hello 世界 🌍</p>")
         (result (sanitize html)))
    (is (search "世界" result))
    (is (search "🌍" result))))

(test test-html-entities
  "Test that HTML entities are preserved"
  (let* ((html "<p>&lt;script&gt; &amp; &quot;</p>")
         (result (sanitize html)))
    (is (search "&lt;" result))
    (is (search "&amp;" result))
    (is (search "&quot;" result))))

;;; Performance / stress tests

(test test-large-html
  "Test that large HTML documents can be sanitized"
  (let* ((large-html (with-output-to-string (s)
                       (dotimes (i 1000)
                         (format s "<p>Paragraph ~D with <strong>bold</strong> text.</p>" i)))))
    (finishes (sanitize large-html))
    (let ((result (sanitize large-html)))
      (is (> (length result) 0))
      (is (search "<p>" result)))))

;;; Utility function tests

(test test-safe-url-p
  "Test the safe-url-p utility function"
  (is (safe-url-p "http://example.com"))
  (is (safe-url-p "https://example.com"))
  (is (safe-url-p "mailto:test@example.com"))
  (is (safe-url-p "/relative/path"))
  (is (safe-url-p "#anchor"))
  (is (not (safe-url-p "javascript:alert(1)")))
  (is (not (safe-url-p "data:text/html,<script>alert(1)</script>"))))
