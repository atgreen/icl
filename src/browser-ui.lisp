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

(defun browser-html ()
  "Return the main HTML page for the browser.
   Loads external CSS and JavaScript from assets directory."
  (format nil "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>ICL Browser</title>
  <link rel='stylesheet' href='/assets/dockview.css'>
  <link rel='stylesheet' href='/assets/xterm.css'>
  <link rel='stylesheet' href='/assets/browser.css'>
  <link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/github-dark.min.css' id='hljs-theme'>
</head>
<body>
  <div id='layout-container'></div>

  <!-- External library scripts -->
  <script src='/assets/dockview.min.js'></script>
  <script src='/assets/xterm.min.js'></script>
  <script src='/assets/xterm-addon-fit.min.js'></script>
  <script src='/assets/viz-standalone.js'></script>
  <script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js'></script>
  <script src='https://cdn.jsdelivr.net/npm/vega@5'></script>
  <script src='https://cdn.jsdelivr.net/npm/vega-lite@5'></script>
  <script src='https://cdn.jsdelivr.net/npm/vega-embed@6'></script>
  <script src='https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js'></script>

  <!-- Configuration (injected by server) -->
  <script>
    const ICL_CONFIG = { wsToken: '~A' };
  </script>

  <!-- Main application script -->
  <script src='/assets/browser.js'></script>
</body>
</html>" *browser-token*))
