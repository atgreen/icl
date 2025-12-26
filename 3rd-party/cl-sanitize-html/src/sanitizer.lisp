;;; sanitizer.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green
;;;
;;; Core HTML sanitization logic

(in-package #:sanitize-html)

(defun sanitize-html (html-string &optional (policy *default-policy*))
  "Sanitize HTML-STRING according to POLICY. Returns sanitized HTML string.
   This is the main entry point for HTML sanitization."
  (when (null html-string)
    (return-from sanitize-html ""))

  (handler-case
      (let ((root (plump:parse html-string)))
        ;; Sanitize all children of root
        (sanitize-node root policy)
        ;; Serialize back to HTML
        (plump:serialize root nil))
    (error (e)
      ;; If parsing fails, return empty string for safety
      (format *error-output* "HTML sanitization error: ~A~%" e)
      "")))

(defun sanitize (html-string &optional (policy *default-policy*))
  "Alias for SANITIZE-HTML"
  (sanitize-html html-string policy))

(defgeneric sanitize-node (node policy)
  (:documentation "Sanitize a Plump DOM node according to policy"))

(defmethod sanitize-node ((node plump:root) policy)
  "Sanitize all children of root node"
  (let ((children (plump:children node)))
    ;; Walk children backwards so removals don't disturb upcoming indices.
    (loop for i from (1- (length children)) downto 0
          for child = (aref children i)
          do (sanitize-node child policy))))

(defmethod sanitize-node ((node plump:element) policy)
  "Sanitize an HTML element node"
  (let ((tag-name (plump:tag-name node)))
    (cond
      ;; Tag is not allowed - remove it
      ((not (tag-allowed-p policy tag-name))
       ;; For dangerous tags (script, style, form elements), remove entirely including content
       ;; For other tags, just remove the tag but keep children
       (if (member (string-downcase tag-name)
                   '("script" "style" "noscript" "form" "input" "button"
                     "textarea" "select" "option" "optgroup" "fieldset" "legend")
                   :test #'string-equal)
           (plump:remove-child node)
           (remove-element-keep-children node)))

      ;; Tag is allowed - sanitize attributes and recurse to children
      (t
       (sanitize-attributes node policy)
       (let ((children (plump:children node)))
         ;; Iterate in reverse order because recursive sanitizing may mutate children.
         (loop for i from (1- (length children)) downto 0
               for child = (aref children i)
               do (sanitize-node child policy)))))))

(defmethod sanitize-node ((node plump:text-node) policy)
  "Text nodes are always safe, no action needed"
  (declare (ignore policy))
  node)

(defmethod sanitize-node ((node plump:comment) policy)
  "Remove or keep comment nodes based on policy"
  (when (policy-remove-comments policy)
    (plump:remove-child node)))

(defmethod sanitize-node ((node plump:cdata) policy)
  "Handle CDATA sections based on policy"
  (if (policy-escape-cdata policy)
      ;; Convert CDATA to text node
      (let ((text (plump:text node)))
        (plump:make-text-node (plump:parent node) text)
        (plump:remove-child node))
      ;; Keep CDATA as-is
      node))

(defmethod sanitize-node (node policy)
  "Default case for unknown node types - remove them"
  (declare (ignore policy))
  (when (plump:parent node)
    (plump:remove-child node)))

(defun remove-element-keep-children (element)
  "Remove ELEMENT but keep its children in the same position"
  (let ((parent (plump:parent element))
        (children (coerce (plump:children element) 'list)))
    (when parent
      ;; Insert each child before the element
      (dolist (child children)
        (plump:insert-before element child))
      ;; Remove the element
      (plump:remove-child element))))

(defun sanitize-attributes (element policy)
  "Remove disallowed attributes from ELEMENT according to POLICY"
  (let* ((tag-name (plump:tag-name element))
         (attrs (plump:attributes element))
         (attr-names nil))

    ;; Collect attribute names (attrs is a hash table)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k attr-names))
             attrs)

    ;; Remove disallowed attributes
    (dolist (attr-name attr-names)
      (unless (attribute-allowed-p policy tag-name attr-name)
        (plump:remove-attribute element attr-name)))

    ;; Sanitize URL attributes
    (sanitize-url-attribute element "href" policy)
    (sanitize-url-attribute element "src" policy)
    (sanitize-url-attribute element "cite" policy)

    ;; Sanitize style attribute if present
    (when (plump:attribute element "style")
      (sanitize-style-attribute element policy))

    ;; Remove event handler attributes (onclick, onload, etc.)
    (remove-event-handlers element)

    ;; Set safe defaults for certain attributes
    (set-safe-defaults element)))

(defun sanitize-url-attribute (element attr-name policy)
  "Sanitize URL in attribute ATTR-NAME of ELEMENT"
  (when-let ((url (plump:attribute element attr-name)))
    (unless (protocol-allowed-p policy url)
      (plump:remove-attribute element attr-name))))

(defun sanitize-style-attribute (element policy)
  "Sanitize inline CSS in style attribute"
  (let ((style (plump:attribute element "style"))
        (allowed-props (policy-allowed-css-properties policy)))
    (if (null allowed-props)
        ;; No CSS properties allowed, remove style
        (plump:remove-attribute element "style")
        ;; Parse and filter CSS properties
        (let ((sanitized-style (sanitize-css style allowed-props)))
          (if (and sanitized-style (> (length sanitized-style) 0))
              (plump:set-attribute element "style" sanitized-style)
              (plump:remove-attribute element "style"))))))

(defun sanitize-css (css-string allowed-properties)
  "Sanitize CSS string, keeping only allowed properties"
  (when (null css-string)
    (return-from sanitize-css ""))

  (let ((properties nil)
        (parts (cl-ppcre:split ";" css-string)))
    (dolist (part parts)
      (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) part))
             (colon-pos (position #\: trimmed)))
        (when (and colon-pos (> colon-pos 0))
          (let* ((prop-name (string-trim '(#\Space #\Tab)
                                        (subseq trimmed 0 colon-pos)))
                 (prop-value (string-trim '(#\Space #\Tab)
                                         (subseq trimmed (1+ colon-pos)))))
            ;; Check if property is allowed and validate value
            (when (and (member (string-downcase prop-name)
                              allowed-properties
                              :test #'string-equal)
                       (not (or (search "javascript:" (string-downcase prop-value))
                               (search "expression" (string-downcase prop-value))
                               (search "import" (string-downcase prop-value))
                               (search "@import" (string-downcase prop-value)))))
              (push (format nil "~A: ~A" prop-name prop-value) properties))))))

    (format nil "~{~A~^; ~}" (nreverse properties))))

(defun remove-event-handlers (element)
  "Remove all event handler attributes (onclick, onload, etc.)"
  (let ((attrs (plump:attributes element))
        (handlers nil))
    ;; Collect event handler attribute names
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (>= (length k) 2)
                         (string-equal "on" (subseq k 0 2)))
                 (push k handlers)))
             attrs)
    ;; Remove them
    (dolist (attr-name handlers)
      (plump:remove-attribute element attr-name))))

(defun set-safe-defaults (element)
  "Set safe default attributes on certain elements"
  (let ((tag-name (plump:tag-name element)))
    ;; Links should open in new window and have safe rel
    (when (and (string-equal tag-name "a")
               (plump:attribute element "href"))
      ;; Set rel="noopener noreferrer" for security
      (if-let ((existing-rel (plump:attribute element "rel")))
        (unless (or (search "noopener" existing-rel)
                    (search "noreferrer" existing-rel))
          (plump:set-attribute element "rel"
                               (format nil "~A noopener noreferrer" existing-rel)))
        (plump:set-attribute element "rel" "noopener noreferrer"))
      ;; Set target="_blank" if not present
      (unless (plump:attribute element "target")
        (plump:set-attribute element "target" "_blank")))))

;;; Utility functions

(defun safe-url-p (url &optional (policy *default-policy*))
  "Check if URL is safe according to POLICY"
  (protocol-allowed-p policy url))

(defun sanitize-url (url &optional (policy *default-policy*))
  "Return URL if safe, nil otherwise"
  (when (safe-url-p url policy)
    url))
