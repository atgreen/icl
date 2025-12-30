;;; tracked-versions.lisp - JavaScript library versions used by ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This file tracks the versions of JavaScript libraries embedded in ICL.
;;; The version checker tool reads this file and queries npm/CDN for updates.

(in-package :icl-version-checker)

(defparameter *tracked-libraries*
  '(;; Monaco Editor - code editor component
    (:name "monaco-editor"
     :current-version "0.52.0"
     :npm-package "monaco-editor"
     :cdn-url "https://cdn.jsdelivr.net/npm/monaco-editor@VERSION/min/vs"
     :notes "Core code editor for browser visualization")

    ;; Mermaid - diagram rendering
    (:name "mermaid"
     :current-version "11.4.1"
     :npm-package "mermaid"
     :cdn-url "https://cdn.jsdelivr.net/npm/mermaid@VERSION/dist/mermaid.esm.min.mjs"
     :notes "Flowcharts, sequence diagrams, etc.")

    ;; Cytoscape - graph visualization
    (:name "cytoscape"
     :current-version "3.30.4"
     :npm-package "cytoscape"
     :cdn-url "https://cdn.jsdelivr.net/npm/cytoscape@VERSION/dist/cytoscape.min.js"
     :notes "Interactive graph/network visualization")

    ;; Viz.js - GraphViz rendering
    (:name "viz.js"
     :current-version "3.11.0"
     :npm-package "@viz-js/viz"
     :cdn-url "https://cdn.jsdelivr.net/npm/@viz-js/viz@VERSION/lib/viz-standalone.js"
     :notes "GraphViz DOT language rendering")

    ;; Vega - declarative visualization grammar
    (:name "vega"
     :current-version "5.30.0"
     :npm-package "vega"
     :cdn-url "https://cdn.jsdelivr.net/npm/vega@VERSION"
     :notes "Core Vega visualization library")

    ;; Vega-Lite - high-level Vega grammar
    (:name "vega-lite"
     :current-version "5.21.0"
     :npm-package "vega-lite"
     :cdn-url "https://cdn.jsdelivr.net/npm/vega-lite@VERSION"
     :notes "Simplified Vega for common chart types")

    ;; Vega-Embed - embedding Vega visualizations
    (:name "vega-embed"
     :current-version "6.26.0"
     :npm-package "vega-embed"
     :cdn-url "https://cdn.jsdelivr.net/npm/vega-embed@VERSION"
     :notes "Embedding helper for Vega/Vega-Lite")

    ;; Plotly - scientific charting
    (:name "plotly.js"
     :current-version "2.35.3"
     :npm-package "plotly.js-dist-min"
     :cdn-url "https://cdn.plot.ly/plotly-VERSION.min.js"
     :notes "Scientific charts, 3D plots, etc.")

    ;; Three.js - 3D graphics
    (:name "three.js"
     :current-version "0.170.0"
     :npm-package "three"
     :cdn-url "https://cdn.jsdelivr.net/npm/three@VERSION/build/three.module.js"
     :notes "3D graphics and WebGL")

    ;; Leaflet - interactive maps
    (:name "leaflet"
     :current-version "1.9.4"
     :npm-package "leaflet"
     :cdn-url "https://cdn.jsdelivr.net/npm/leaflet@VERSION/dist/leaflet.js"
     :notes "Interactive maps")

    ;; Marked - Markdown parsing
    (:name "marked"
     :current-version "15.0.4"
     :npm-package "marked"
     :cdn-url "https://cdn.jsdelivr.net/npm/marked@VERSION/marked.min.js"
     :notes "Markdown to HTML conversion")

    ;; Highlight.js - syntax highlighting
    (:name "highlight.js"
     :current-version "11.10.0"
     :npm-package "highlight.js"
     :cdn-url "https://cdn.jsdelivr.net/npm/highlight.js@VERSION/lib/core.min.js"
     :notes "Code syntax highlighting")

    ;; KaTeX - LaTeX math rendering
    (:name "katex"
     :current-version "0.16.11"
     :npm-package "katex"
     :cdn-url "https://cdn.jsdelivr.net/npm/katex@VERSION/dist/katex.min.js"
     :notes "Fast LaTeX math rendering")

    ;; Dockview - panel layout system
    (:name "dockview"
     :current-version "4.3.1"
     :npm-package "dockview-core"
     :cdn-url "https://cdn.jsdelivr.net/npm/dockview-core@VERSION/dist/dockview-core.esm.js"
     :notes "Dockable panel layout manager"))
  "List of JavaScript libraries tracked for version updates.
Each entry is a plist with:
  :name           - Human-readable library name
  :current-version - Version currently used in ICL
  :npm-package    - npm package name for registry lookup
  :cdn-url        - CDN URL template (VERSION is placeholder)
  :notes          - Brief description of usage")
