;;; policies.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green
;;;
;;; Sanitization policies (OWASP-style whitelists)

(in-package #:sanitize-html)

(defstruct policy
  "Sanitization policy defining what HTML elements, attributes, and protocols are allowed"
  (allowed-tags nil :type list)
  (allowed-attributes nil :type list)
  (allowed-protocols '("http" "https" "mailto") :type list)
  (allowed-css-properties nil :type list)
  (remove-comments t :type boolean)
  (escape-cdata t :type boolean))

;;; Default policy - balanced security and usability
(defparameter *default-policy*
  (make-policy
   :allowed-tags
   '("a" "abbr" "b" "blockquote" "br" "caption" "code" "dd" "del" "div"
     "dl" "dt" "em" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "i" "img" "ins"
     "kbd" "li" "ol" "p" "pre" "q" "s" "samp" "small" "span" "strike"
     "strong" "sub" "sup" "table" "tbody" "td" "tfoot" "th" "thead" "tr"
     "u" "ul" "var")

   :allowed-attributes
   '(("a" . ("href" "title" "rel"))
     ("abbr" . ("title"))
     ("blockquote" . ("cite"))
     ("img" . ("src" "alt" "title" "width" "height"))
     ("q" . ("cite"))
     ("td" . ("colspan" "rowspan" "headers"))
     ("th" . ("colspan" "rowspan" "headers" "scope"))
     ("*" . ("class" "id" "dir" "lang" "title")))  ; Global attributes

   :allowed-protocols
   '("http" "https" "mailto" "ftp")

   :remove-comments t
   :escape-cdata t)
  "Default sanitization policy - balanced for general content")

;;; Strict policy - maximum security
(defparameter *strict-policy*
  (make-policy
   :allowed-tags
   '("a" "b" "blockquote" "br" "code" "em" "i" "li" "ol" "p" "pre"
     "span" "strong" "ul")

   :allowed-attributes
   '(("a" . ("href" "title"))
     ("*" . ("class")))

   :allowed-protocols
   '("https" "mailto")

   :remove-comments t
   :escape-cdata t)
  "Strict policy - minimal allowed HTML")

;;; Email policy - designed for HTML emails
(defparameter *email-policy*
  (make-policy
   :allowed-tags
   '("a" "abbr" "b" "blockquote" "br" "caption" "center" "code" "dd" "del"
     "div" "dl" "dt" "em" "font" "h1" "h2" "h3" "h4" "h5" "h6" "hr" "i"
     "img" "ins" "li" "ol" "p" "pre" "q" "s" "small" "span" "strike"
     "strong" "sub" "sup" "table" "tbody" "td" "tfoot" "th" "thead" "tr"
     "tt" "u" "ul")

   :allowed-attributes
   '(("a" . ("href" "title" "rel" "target"))
     ("img" . ("src" "alt" "title" "width" "height" "align" "border"))
     ("table" . ("border" "cellpadding" "cellspacing" "width" "align"))
     ("td" . ("colspan" "rowspan" "width" "height" "align" "valign" "bgcolor"))
     ("th" . ("colspan" "rowspan" "width" "height" "align" "valign" "bgcolor"))
     ("tr" . ("align" "valign" "bgcolor"))
     ("font" . ("color" "face" "size"))
     ("div" . ("align"))
     ("p" . ("align"))
     ("h1" . ("align"))
     ("h2" . ("align"))
     ("h3" . ("align"))
     ("h4" . ("align"))
     ("h5" . ("align"))
     ("h6" . ("align"))
     ("hr" . ("width" "size" "align" "noshade"))
     ("*" . ("class" "id" "style")))  ; Allow inline styles for email formatting

   :allowed-protocols
   '("http" "https" "mailto" "cid" "data")  ; cid for inline images, data for base64

   ;; Email-specific CSS properties (limited set)
   ;; Excludes dangerous properties: position, z-index, float, transform, etc.
   :allowed-css-properties
   '("color" "background-color" "font-size" "font-family" "font-weight"
     "font-style" "text-align" "text-decoration" "margin" "padding"
     "border" "border-collapse" "border-spacing" "vertical-align"
     "width" "min-width" "max-width"
     "height" "min-height" "max-height"
     "line-height" "letter-spacing" "word-spacing"
     "text-indent" "white-space")

   :remove-comments t
   :escape-cdata t)
  "Email policy - allows common email HTML formatting")

(defun get-allowed-attributes (policy tag-name)
  "Get list of allowed attributes for TAG-NAME according to POLICY"
  (let ((tag-name-lower (string-downcase tag-name))
        (attrs (policy-allowed-attributes policy)))
    (append
     ;; Tag-specific attributes
     (rest (assoc tag-name-lower attrs :test #'string-equal))
     ;; Global attributes (wildcard)
     (rest (assoc "*" attrs :test #'string-equal)))))

(defun tag-allowed-p (policy tag-name)
  "Check if TAG-NAME is allowed by POLICY"
  (member (string-downcase tag-name)
          (policy-allowed-tags policy)
          :test #'string-equal))

(defun attribute-allowed-p (policy tag-name attr-name)
  "Check if ATTR-NAME is allowed for TAG-NAME by POLICY"
  (let ((allowed (get-allowed-attributes policy tag-name)))
    (member (string-downcase attr-name)
            allowed
            :test #'string-equal)))

(defun protocol-allowed-p (policy url)
  "Check if URL uses an allowed protocol according to POLICY"
  (when (and url (stringp url))
    (let ((url-lower (string-downcase url)))
      (or
       ;; Relative URLs (no protocol)
       (and (> (length url-lower) 0)
            (char= (char url-lower 0) #\/))
       ;; Fragment identifiers
       (and (> (length url-lower) 0)
            (char= (char url-lower 0) #\#))
       ;; Check protocol
       (some (lambda (proto)
               (let ((prefix (concatenate 'string proto ":")))
                 (and (>= (length url-lower) (length prefix))
                      (string= url-lower prefix :end1 (length prefix)))))
             (policy-allowed-protocols policy))))))
