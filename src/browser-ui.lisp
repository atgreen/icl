;;; browser-ui.lisp --- HTML generation for ICL browser
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides the browser-html function that generates
;;; the main HTML page shell. The actual CSS and JavaScript are
;;; loaded from external files in the assets directory.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; HTTP Handlers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %hl-color (accessor default)
  "The active theme's ACCESSOR colour as a #RRGGBB string, or DEFAULT."
  (let ((v (and *current-terminal-theme* (funcall accessor *current-terminal-theme*))))
    (if (and (stringp v) (plusp (length v)) (char= (char v 0) #\#)) v default)))

(defun %hl-colors-json ()
  "JSON of the active theme's syntax-highlight colours, for the browser's resting
   cell highlighter so it matches the ICL editor's colours."
  (format nil "{\"keyword\":\"~A\",\"string\":\"~A\",\"comment\":\"~A\",~
               \"number\":\"~A\",\"package\":\"~A\",\"special\":\"~A\",\"paren\":\"~A\"}"
          (%hl-color #'terminal-theme-hl-keyword "#FF79C6")
          (%hl-color #'terminal-theme-hl-string  "#50FA7B")
          (%hl-color #'terminal-theme-hl-comment "#6272A4")
          (%hl-color #'terminal-theme-hl-number  "#FFB86C")
          (%hl-color #'terminal-theme-hl-package "#8BE9FD")
          (%hl-color #'terminal-theme-hl-special "#BD93F9")
          (%hl-color #'terminal-theme-hl-paren   "#A8A8A8")))

(defun browser-html ()
  "Return the main HTML page for the browser.
   Loads external CSS and JavaScript from assets directory."
  (format nil "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>ICL Browser</title>
  <link rel='icon' type='image/x-icon' href='/assets/favicon.ico'>
  <link rel='icon' type='image/png' sizes='16x16' href='/assets/favicon-16.png'>
  <link rel='icon' type='image/png' sizes='32x32' href='/assets/favicon-32.png'>
  <link rel='icon' type='image/png' sizes='48x48' href='/assets/favicon-48.png'>
  <link rel='icon' type='image/png' sizes='192x192' href='/assets/favicon-192.png'>
  <link rel='apple-touch-icon' href='/assets/apple-touch-icon.png'>
  <link rel='stylesheet' href='/assets/dockview.css'>
  <link rel='stylesheet' href='/assets/xterm.css'>
  <link rel='stylesheet' href='/assets/browser.css'>
  <link rel='stylesheet' href='/assets/katex/katex.min.css'>
</head>
<body data-ws-token='~A' data-version='~A' data-unsafe-visualizations='~A' data-hl-colors='~A'>
  <div id='layout-container'></div>

  <!-- External library scripts -->
  <script src='/assets/dockview.min.js'></script>
  <script src='/assets/xterm.min.js'></script>
  <script src='/assets/xterm-addon-fit.min.js'></script>
  <script src='/assets/viz-standalone.js'></script>
  <script src='/assets/vega.min.js'></script>
  <script src='/assets/vega-lite.min.js'></script>
  <script src='/assets/vega-embed.min.js'></script>
  <script src='/assets/mermaid.min.js'></script>
  <script src='/assets/katex/katex.min.js'></script>

  <!-- Monaco loader MUST be BEFORE regulex to avoid AMD conflicts.
       Load Monaco loader, configure and START loading immediately, then regulex can override AMD globals -->
  <script src='/assets/monaco/vs/loader.js'></script>
  <script src='/assets/monaco-init.js'></script>
  <script src='/assets/regulex.js'></script>

  <!-- Main application script -->
  <script src='/assets/browser.js'></script>
  <script src='/assets/notebook.js'></script>
</body>
</html>" *browser-token* +version+ (if *unsafe-visualizations* "true" "false")
          (%hl-colors-json)))
