;;; tracked-versions.lisp - JavaScript library versions used by ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This file tracks the versions of JavaScript libraries embedded in ICL.
;;; The version checker tool reads this file and queries npm/CDN for updates.
;;;
;;; Keep in sync with assets/WEB-LICENSES

(in-package :icl-version-checker)

(defparameter *tracked-libraries*
  '(;; Dockview - panel layout system
    (:name "dockview-core"
     :current-version "4.12.0"
     :npm-package "dockview-core"
     :notes "Dockable panel layout manager")

    ;; xterm.js - terminal emulator
    (:name "xterm.js"
     :current-version "5.3.0"
     :npm-package "xterm"
     :notes "Terminal emulator for browser")

    ;; xterm-addon-fit - terminal sizing
    (:name "xterm-addon-fit"
     :current-version "0.8.0"
     :npm-package "@xterm/addon-fit"
     :notes "Auto-fit terminal to container")

    ;; Vega - declarative visualization grammar
    (:name "vega"
     :current-version "5.30.0"
     :npm-package "vega"
     :notes "Core Vega visualization library")

    ;; Vega-Lite - high-level Vega grammar
    (:name "vega-lite"
     :current-version "5.21.0"
     :npm-package "vega-lite"
     :notes "Simplified Vega for common chart types")

    ;; Vega-Embed - embedding Vega visualizations
    (:name "vega-embed"
     :current-version "6.26.0"
     :npm-package "vega-embed"
     :notes "Embedding helper for Vega/Vega-Lite")

    ;; Mermaid - diagram rendering
    (:name "mermaid"
     :current-version "11.4.1"
     :npm-package "mermaid"
     :notes "Flowcharts, sequence diagrams, etc.")

    ;; Monaco Editor - code editor component
    (:name "monaco-editor"
     :current-version "0.52.0"
     :npm-package "monaco-editor"
     :notes "Code editor for browser visualization")

    ;; Regulex - regex visualization (no npm package, hosted on jex.im)
    ;; Note: Regulex bundles Raphaël 2.1.2
    (:name "regulex"
     :current-version "0.0.5"
     :npm-package nil
     :notes "Regex visualization (hosted, not on npm)"))
  "List of JavaScript libraries tracked for version updates.
Each entry is a plist with:
  :name           - Human-readable library name
  :current-version - Version currently used in ICL
  :npm-package    - npm package name for registry lookup (NIL if not on npm)
  :notes          - Brief description of usage")
