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
  <link rel='icon' type='image/x-icon' href='/assets/favicon.ico'>
  <link rel='icon' type='image/png' sizes='16x16' href='/assets/favicon-16.png'>
  <link rel='icon' type='image/png' sizes='32x32' href='/assets/favicon-32.png'>
  <link rel='icon' type='image/png' sizes='48x48' href='/assets/favicon-48.png'>
  <link rel='icon' type='image/png' sizes='192x192' href='/assets/favicon-192.png'>
  <link rel='apple-touch-icon' href='/assets/apple-touch-icon.png'>
  <link rel='stylesheet' href='/assets/dockview.css'>
  <link rel='stylesheet' href='/assets/xterm.css'>
  <link rel='stylesheet' href='/assets/browser.css'>
</head>
<body data-ws-token='~A' data-version='~A' data-unsafe-visualizations='~A'>
  <div id='layout-container'></div>

  <!-- External library scripts -->
  <script src='/assets/dockview.min.js'></script>
  <script src='/assets/xterm.min.js'></script>
  <script src='/assets/xterm-addon-fit.min.js'></script>
  <script src='/assets/viz-standalone.js'></script>
  <script src='/assets/monaco/vs/loader.js'></script>
  <script src='/assets/vega.min.js'></script>
  <script src='/assets/vega-lite.min.js'></script>
  <script src='/assets/vega-embed.min.js'></script>
  <script src='/assets/mermaid.min.js'></script>
  <script src='/assets/regulex.js'></script>

  <!-- Main application script -->
  <script src='/assets/browser.js'></script>
</body>
</html>" *browser-token* +version+ (if *unsafe-visualizations* "true" "false")))
