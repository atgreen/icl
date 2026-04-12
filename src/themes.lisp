;;; themes.lisp --- Theming system for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Theme Structure
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct terminal-theme
  "Theme definition for terminal/TUI interface.
   Colors are hex strings (e.g. \"#FF79C6\") or legacy 256-color integers."
  (name nil :type (or keyword null))
  (display-name "" :type string)
  (dark-p t :type boolean)
  ;; Syntax highlighting colors (hex strings)
  (hl-keyword "#FF79C6" :type (or string fixnum))  ; Keywords like DEFUN, LET
  (hl-string "#50FA7B" :type (or string fixnum))    ; String literals
  (hl-comment "#6272A4" :type (or string fixnum))   ; Comments
  (hl-number "#FFB86C" :type (or string fixnum))    ; Numeric literals
  (hl-symbol "#F8F8F2" :type (or string fixnum))    ; Regular symbols
  (hl-package "#8BE9FD" :type (or string fixnum))   ; Package prefixes
  (hl-special "#BD93F9" :type (or string fixnum))   ; Special forms
  (hl-paren "#A8A8A8" :type (or string fixnum))     ; Parentheses
  ;; Paren matching background colors
  (hl-paren-match-bg nil :type (or string fixnum null))    ; Matching paren background
  (hl-paren-mismatch-bg nil :type (or string fixnum null)) ; Mismatched paren background
  ;; Text style flags for syntax elements
  (comment-italic t :type boolean)                  ; Render comments in italic
  (special-bold t :type boolean)                    ; Render special forms in bold
  (keyword-bold nil :type boolean)                  ; Render keywords in bold
  ;; UI colors
  (prompt-primary "#BD93F9" :type (or string fixnum))  ; Main prompt color
  (prompt-package "#6272A4" :type (or string fixnum))  ; Package name in prompt
  (error "#FF5555" :type (or string fixnum))           ; Error messages
  (warning "#FFB86C" :type (or string fixnum))         ; Warnings
  (info "#8BE9FD" :type (or string fixnum))            ; Info messages
  (dim "#6272A4" :type (or string fixnum)))             ; Dimmed/secondary text

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Browser Theme Structure
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct browser-theme
  "Theme definition for browser interface.
   Colors are CSS hex values (strings)."
  (name nil :type (or keyword null))
  (display-name "" :type string)
  (dark-p t :type boolean)
  ;; Main UI colors
  (bg-primary "#1e1e2e" :type string)
  (bg-secondary "#313244" :type string)
  (bg-tertiary "#45475a" :type string)
  (fg-primary "#cdd6f4" :type string)
  (fg-secondary "#bac2de" :type string)
  (fg-muted "#6c7086" :type string)
  (accent "#89b4fa" :type string)
  (accent-hover "#b4befe" :type string)
  (border "#45475a" :type string)
  ;; Syntax highlighting
  (syntax-keyword "#cba6f7" :type string)
  (syntax-string "#a6e3a1" :type string)
  (syntax-comment "#6c7086" :type string)
  (syntax-number "#fab387" :type string)
  (syntax-symbol "#cdd6f4" :type string)
  (syntax-package "#89dceb" :type string)
  (syntax-special "#f5c2e7" :type string)
  (syntax-error "#f38ba8" :type string)
  ;; xterm.js ANSI palette (16 colors)
  (ansi-black "#45475a" :type string)
  (ansi-red "#f38ba8" :type string)
  (ansi-green "#a6e3a1" :type string)
  (ansi-yellow "#f9e2af" :type string)
  (ansi-blue "#89b4fa" :type string)
  (ansi-magenta "#cba6f7" :type string)
  (ansi-cyan "#89dceb" :type string)
  (ansi-white "#bac2de" :type string)
  (ansi-bright-black "#585b70" :type string)
  (ansi-bright-red "#f38ba8" :type string)
  (ansi-bright-green "#a6e3a1" :type string)
  (ansi-bright-yellow "#f9e2af" :type string)
  (ansi-bright-blue "#89b4fa" :type string)
  (ansi-bright-magenta "#cba6f7" :type string)
  (ansi-bright-cyan "#89dceb" :type string)
  (ansi-bright-white "#a6adc8" :type string))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Registries
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-themes* (make-hash-table :test #'eq)
  "Registry of terminal themes by name (keyword).")

(defvar *browser-themes* (make-hash-table :test #'eq)
  "Registry of browser themes by name (keyword).")

(defvar *current-terminal-theme* nil
  "Currently active terminal theme.")

(defvar *current-browser-theme* nil
  "Currently active browser theme.")

(defvar *default-dark-terminal-theme* :dracula
  "Default terminal theme for dark mode.")

(defvar *default-light-terminal-theme* :github-light
  "Default terminal theme for light mode.")

(defvar *default-dark-browser-theme* :dracula
  "Default browser theme for dark mode.")

(defvar *default-light-browser-theme* :github-light
  "Default browser theme for light mode.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Registration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun register-terminal-theme (theme)
  "Register a terminal theme."
  (setf (gethash (terminal-theme-name theme) *terminal-themes*) theme))

(defun register-browser-theme (theme)
  "Register a browser theme."
  (setf (gethash (browser-theme-name theme) *browser-themes*) theme))

(defun find-terminal-theme (name)
  "Find a terminal theme by name (keyword)."
  (gethash name *terminal-themes*))

(defun find-browser-theme (name)
  "Find a browser theme by name (keyword)."
  (gethash name *browser-themes*))

(defun list-terminal-themes ()
  "List all registered terminal themes."
  (let ((themes nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v themes))
             *terminal-themes*)
    (sort themes #'string< :key #'terminal-theme-display-name)))

(defun list-browser-themes ()
  "List all registered browser themes."
  (let ((themes nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v themes))
             *browser-themes*)
    (sort themes #'string< :key #'browser-theme-display-name)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dark Mode Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-dark-mode-override* :auto
  "User override for terminal dark mode detection.
   Set to T for dark, NIL for light, or :auto for auto-detection.")

;; query-terminal-background is defined in terminal-posix.lisp or terminal-windows.lisp
;; Platform-specific implementation handles raw terminal I/O correctly

(defun detect-terminal-dark-mode ()
  "Detect if the terminal prefers dark mode.
   Returns T for dark mode, NIL for light mode."
  ;; Check user override first
  (when (and (boundp '*terminal-dark-mode-override*)
             (not (eq *terminal-dark-mode-override* :auto)))
    (return-from detect-terminal-dark-mode *terminal-dark-mode-override*))
  ;; Check COLORFGBG (format: 'foreground;background')
  ;; Background >= 8 typically means light (white is 15)
  (let ((colorfgbg (uiop:getenv "COLORFGBG")))
    (when (and colorfgbg (plusp (length colorfgbg)))
      (let ((parts (uiop:split-string colorfgbg :separator ";")))
        (when (>= (length parts) 2)
          (let ((bg (ignore-errors (parse-integer (second parts)))))
            (when bg
              (return-from detect-terminal-dark-mode (< bg 8))))))))
  ;; Try OSC 11 query via raw mode (enter/exit raw mode briefly)
  (handler-case
      (multiple-value-bind (r g b) (query-terminal-background)
        (when (and r g b)
          (let ((luminance (compute-luminance r g b)))
            (return-from detect-terminal-dark-mode (< luminance 0.5)))))
    (error () nil))
  ;; Check GNOME/GTK dark mode preference
  (let ((gtk-theme (uiop:getenv "GTK_THEME")))
    (when (and gtk-theme (search "dark" gtk-theme :test #'char-equal))
      (return-from detect-terminal-dark-mode t)))
  ;; Default to dark mode — most terminal users have dark backgrounds
  t)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Application
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %ensure-hex (val)
  "If VAL is an integer (legacy 256-color index), convert to hex string.
   If already a string, return as-is."
  (if (integerp val)
      (tuition:ansi256-to-hex val)
      val))

(defun %make-theme-color (val)
  "Create a tuition:complete-color from a theme value.
   Accepts hex strings or legacy 256-color integers."
  (tuition:make-complete-color :truecolor (%ensure-hex val)))

(defun apply-terminal-theme (theme)
  "Apply a terminal theme, updating all color variables."
  (when (keywordp theme)
    (setf theme (find-terminal-theme theme)))
  (unless theme
    (error "Terminal theme not found"))
  (setf *current-terminal-theme* theme)
  ;; Invalidate cached markdown style
  (setf *current-markdown-style* nil)
  ;; Update syntax highlighting colors (as complete-color objects)
  (setf *hl-keyword-color* (%make-theme-color (terminal-theme-hl-keyword theme))
        *hl-string-color* (%make-theme-color (terminal-theme-hl-string theme))
        *hl-comment-color* (%make-theme-color (terminal-theme-hl-comment theme))
        *hl-number-color* (%make-theme-color (terminal-theme-hl-number theme))
        *hl-symbol-color* (%make-theme-color (terminal-theme-hl-symbol theme))
        *hl-package-color* (%make-theme-color (terminal-theme-hl-package theme))
        *hl-special-color* (%make-theme-color (terminal-theme-hl-special theme))
        *hl-paren-color* (%make-theme-color (terminal-theme-hl-paren theme)))
  ;; Update paren-match background colors
  (let ((match-bg (terminal-theme-hl-paren-match-bg theme))
        (mismatch-bg (terminal-theme-hl-paren-mismatch-bg theme)))
    (setf *hl-paren-match-bg-color* (when match-bg (%make-theme-color match-bg))
          *hl-paren-mismatch-bg-color* (when mismatch-bg (%make-theme-color mismatch-bg))))
  ;; Update text style flags
  (setf *comment-italic* (terminal-theme-comment-italic theme)
        *special-bold* (terminal-theme-special-bold theme)
        *keyword-bold* (terminal-theme-keyword-bold theme))
  ;; Update UI colors (as complete-color objects)
  (setf *color-prompt* (%make-theme-color (terminal-theme-prompt-primary theme))
        *color-package* (%make-theme-color (terminal-theme-prompt-package theme))
        *color-error* (%make-theme-color (terminal-theme-error theme))
        *color-warning* (%make-theme-color (terminal-theme-warning theme))
        *color-info* (%make-theme-color (terminal-theme-info theme))
        *color-dim* (%make-theme-color (terminal-theme-dim theme)))
  ;; Regenerate ANSI escape sequences
  (refresh-ansi-codes)
  theme)

(defun apply-browser-theme (theme)
  "Apply a browser theme, notifying all connected clients."
  (when (keywordp theme)
    (setf theme (find-browser-theme theme)))
  (unless theme
    (error "Browser theme not found"))
  (setf *current-browser-theme* theme)
  ;; Broadcast to all connected WebSocket clients
  (broadcast-browser-theme theme)
  theme)

(defun refresh-ansi-codes ()
  "Regenerate ANSI escape code strings from current colors.
   Uses tuition's color resolution for capability-aware output."
  (setf *ansi-prompt* (tuition:resolve-color-foreground *color-prompt*)
        *ansi-package* (tuition:resolve-color-foreground *color-package*)
        *ansi-error* (tuition:resolve-color-foreground *color-error*)
        *ansi-warning* (tuition:resolve-color-foreground *color-warning*)
        *ansi-info* (tuition:resolve-color-foreground *color-info*)
        *ansi-fg-gray* (tuition:resolve-color-foreground *color-dim*))
  ;; Refresh syntax highlighting colors
  (when (fboundp 'refresh-highlight-colors)
    (funcall 'refresh-highlight-colors))
  ;; Refresh value formatting colors (output.lisp)
  (when (fboundp 'refresh-value-colors)
    (funcall 'refresh-value-colors)))

(defun broadcast-browser-theme (theme)
  "Broadcast theme to all connected browser clients.
   This is a no-op if the browser module isn't loaded."
  (declare (ignore theme))
  ;; Will be replaced by browser.lisp
  nil)

(defun auto-select-terminal-theme ()
  "Auto-select terminal theme based on dark mode detection."
  (if (detect-terminal-dark-mode)
      (apply-terminal-theme *default-dark-terminal-theme*)
      (apply-terminal-theme *default-light-terminal-theme*)))

(defun auto-select-browser-theme (&optional dark-p)
  "Auto-select browser theme. DARK-P is T for dark, NIL for light,
   or :unknown if not provided (uses current terminal theme as fallback)."
  (let ((dark (cond
                ((eq dark-p :unknown)
                 ;; Use current terminal theme's dark-p instead of re-querying
                 (if *current-terminal-theme*
                     (terminal-theme-dark-p *current-terminal-theme*)
                     t))
                (t dark-p))))
    (if dark
        (apply-browser-theme *default-dark-browser-theme*)
        (apply-browser-theme *default-light-browser-theme*))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Browser Theme CSS Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun browser-theme-to-css (theme)
  "Generate CSS variable declarations from a browser theme.
   Includes both application variables and Dockview theming variables."
  (let ((bg-primary (browser-theme-bg-primary theme))
        (bg-secondary (browser-theme-bg-secondary theme))
        (bg-tertiary (browser-theme-bg-tertiary theme))
        (fg-primary (browser-theme-fg-primary theme))
        (fg-secondary (browser-theme-fg-secondary theme))
        (fg-muted (browser-theme-fg-muted theme))
        (accent (browser-theme-accent theme))
        (accent-hover (browser-theme-accent-hover theme))
        (border (browser-theme-border theme)))
    (format nil ":root {
  /* Application theme variables */
  --bg-primary: ~A;
  --bg-secondary: ~A;
  --bg-tertiary: ~A;
  --fg-primary: ~A;
  --fg-secondary: ~A;
  --fg-muted: ~A;
  --accent: ~A;
  --accent-hover: ~A;
  --border: ~A;
  --syntax-keyword: ~A;
  --syntax-string: ~A;
  --syntax-comment: ~A;
  --syntax-number: ~A;
  --syntax-symbol: ~A;
  --syntax-package: ~A;
  --syntax-special: ~A;
  --syntax-error: ~A;
}

/* Dockview theming - CSS variables */
#layout-container.icl-dockview-theme {
  --dv-paneview-active-outline-color: ~A;
  --dv-group-view-background-color: ~A;
  --dv-tabs-and-actions-container-background-color: ~A;
  --dv-activegroup-visiblepanel-tab-background-color: ~A;
  --dv-activegroup-hiddenpanel-tab-background-color: ~A;
  --dv-activegroup-visiblepanel-tab-color: ~A;
  --dv-activegroup-hiddenpanel-tab-color: ~A;
  --dv-inactivegroup-visiblepanel-tab-background-color: ~A;
  --dv-inactivegroup-hiddenpanel-tab-background-color: ~A;
  --dv-inactivegroup-visiblepanel-tab-color: ~A;
  --dv-inactivegroup-hiddenpanel-tab-color: ~A;
  --dv-tab-divider-color: ~A;
  --dv-separator-border: ~A;
  --dv-paneview-header-border-color: ~A;
  --dv-tabs-container-scrollbar-color: ~A;
  --dv-icon-hover-background-color: ~A;
  --dv-drag-over-background-color: ~A;
  --dv-drag-over-border-color: ~A;
}

/* Dockview explicit element styling for better compatibility */
.icl-dockview-theme .dv-tabs-and-actions-container {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-tab {
  color: ~A !important;
}
.icl-dockview-theme .dv-tab.dv-active-tab {
  background-color: ~A !important;
  color: ~A !important;
}
.icl-dockview-theme .dv-tab:not(.dv-active-tab) {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-groupview {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-resize-handle-border,
.icl-dockview-theme .dv-tabs-and-actions-container {
  border-color: ~A !important;
}
.icl-dockview-theme .dv-default-tab-action svg {
  fill: ~A !important;
}"
            ;; Application variables
            bg-primary bg-secondary bg-tertiary
            fg-primary fg-secondary fg-muted
            accent accent-hover border
            (browser-theme-syntax-keyword theme)
            (browser-theme-syntax-string theme)
            (browser-theme-syntax-comment theme)
            (browser-theme-syntax-number theme)
            (browser-theme-syntax-symbol theme)
            (browser-theme-syntax-package theme)
            (browser-theme-syntax-special theme)
            (browser-theme-syntax-error theme)
            ;; Dockview CSS variables
            accent                          ; paneview active outline
            bg-primary                      ; group view background
            bg-secondary                    ; tabs container background
            bg-tertiary                     ; active group visible tab bg
            bg-secondary                    ; active group hidden tab bg
            fg-primary                      ; active group visible tab text
            fg-muted                        ; active group hidden tab text
            bg-secondary                    ; inactive group visible tab bg
            bg-secondary                    ; inactive group hidden tab bg
            fg-secondary                    ; inactive group visible tab text
            fg-muted                        ; inactive group hidden tab text
            border                          ; tab divider
            border                          ; separator border
            border                          ; paneview header border
            fg-muted                        ; scrollbar color
            bg-tertiary                     ; icon hover background
            (format nil "~Aaa" accent)      ; drag over bg (with alpha)
            accent                          ; drag over border
            ;; Explicit element styling
            bg-secondary                    ; tabs container bg
            fg-secondary                    ; tab text color
            bg-tertiary                     ; active tab bg
            fg-primary                      ; active tab text
            bg-secondary                    ; inactive tab bg
            bg-primary                      ; groupview bg
            border                          ; borders
            fg-muted)))

(defun browser-theme-to-xterm-options (theme)
  "Generate xterm.js theme options as an alist."
  (list (cons "background" (browser-theme-bg-primary theme))
        (cons "foreground" (browser-theme-fg-primary theme))
        (cons "cursor" (browser-theme-accent theme))
        (cons "cursorAccent" (browser-theme-bg-primary theme))
        (cons "selection" (browser-theme-bg-tertiary theme))
        (cons "black" (browser-theme-ansi-black theme))
        (cons "red" (browser-theme-ansi-red theme))
        (cons "green" (browser-theme-ansi-green theme))
        (cons "yellow" (browser-theme-ansi-yellow theme))
        (cons "blue" (browser-theme-ansi-blue theme))
        (cons "magenta" (browser-theme-ansi-magenta theme))
        (cons "cyan" (browser-theme-ansi-cyan theme))
        (cons "white" (browser-theme-ansi-white theme))
        (cons "brightBlack" (browser-theme-ansi-bright-black theme))
        (cons "brightRed" (browser-theme-ansi-bright-red theme))
        (cons "brightGreen" (browser-theme-ansi-bright-green theme))
        (cons "brightYellow" (browser-theme-ansi-bright-yellow theme))
        (cons "brightBlue" (browser-theme-ansi-bright-blue theme))
        (cons "brightMagenta" (browser-theme-ansi-bright-magenta theme))
        (cons "brightCyan" (browser-theme-ansi-bright-cyan theme))
        (cons "brightWhite" (browser-theme-ansi-bright-white theme))))

(defun alist-to-hash (alist)
  "Convert an alist to a hash table for JSON serialization."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht)
            (if (and (consp (cdr pair)) (consp (car (cdr pair))))
                ;; Nested alist
                (alist-to-hash (cdr pair))
                (cdr pair))))))

(defun browser-theme-to-json (theme)
  "Convert browser theme to JSON for WebSocket transmission."
  (let* ((xterm-alist (browser-theme-to-xterm-options theme))
         (xterm-hash (alist-to-hash xterm-alist))
         (data (make-hash-table :test 'equal)))
    (setf (gethash "name" data) (string-downcase (symbol-name (browser-theme-name theme)))
          (gethash "displayName" data) (browser-theme-display-name theme)
          (gethash "darkP" data) (browser-theme-dark-p theme)
          (gethash "css" data) (browser-theme-to-css theme)
          (gethash "xterm" data) xterm-hash
          ;; Use empty class - our CSS variables will control all styling
          (gethash "dockviewTheme" data) "icl-dockview-theme")
    (com.inuoe.jzon:stringify data)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Custom Theme Definition Macros
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro define-terminal-theme (name &rest args)
  "Define and register a custom terminal theme.
   NAME should be a keyword.
   ARGS are keyword arguments matching terminal-theme slots."
  `(register-terminal-theme
    (make-terminal-theme :name ,name ,@args)))

(defmacro define-browser-theme (name &rest args)
  "Define and register a custom browser theme.
   NAME should be a keyword.
   ARGS are keyword arguments matching browser-theme slots."
  `(register-browser-theme
    (make-browser-theme :name ,name ,@args)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: DRACULA
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :dracula
  :display-name "Dracula"
  :dark-p t
  :hl-keyword "#FF79C6"
  :hl-string "#50FA7B"
  :hl-comment "#6272A4"
  :hl-number "#FFB86C"
  :hl-symbol "#F8F8F2"
  :hl-package "#8BE9FD"
  :hl-special "#BD93F9"
  :hl-paren "#6272A4"
  :hl-paren-match-bg "#44475A"
  :hl-paren-mismatch-bg "#FF5555"
  :prompt-primary "#BD93F9"
  :prompt-package "#6272A4"
  :error "#FF5555"
  :warning "#FFB86C"
  :info "#8BE9FD"
  :dim "#6272A4"))

(register-browser-theme
 (make-browser-theme
  :name :dracula
  :display-name "Dracula"
  :dark-p t
  :bg-primary "#282a36"
  :bg-secondary "#44475a"
  :bg-tertiary "#6272a4"
  :fg-primary "#f8f8f2"
  :fg-secondary "#f8f8f2"
  :fg-muted "#6272a4"
  :accent "#bd93f9"
  :accent-hover "#ff79c6"
  :border "#44475a"
  :syntax-keyword "#ff79c6"
  :syntax-string "#50fa7b"
  :syntax-comment "#6272a4"
  :syntax-number "#ffb86c"
  :syntax-symbol "#f8f8f2"
  :syntax-package "#8be9fd"
  :syntax-special "#bd93f9"
  :syntax-error "#ff5555"
  :ansi-black "#21222c"
  :ansi-red "#ff5555"
  :ansi-green "#50fa7b"
  :ansi-yellow "#f1fa8c"
  :ansi-blue "#bd93f9"
  :ansi-magenta "#ff79c6"
  :ansi-cyan "#8be9fd"
  :ansi-white "#f8f8f2"
  :ansi-bright-black "#6272a4"
  :ansi-bright-red "#ff6e6e"
  :ansi-bright-green "#69ff94"
  :ansi-bright-yellow "#ffffa5"
  :ansi-bright-blue "#d6acff"
  :ansi-bright-magenta "#ff92df"
  :ansi-bright-cyan "#a4ffff"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: NORD
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :nord
  :display-name "Nord"
  :dark-p t
  :hl-keyword "#B48EAD"
  :hl-string "#A3BE8C"
  :hl-comment "#616E88"
  :hl-number "#D08770"
  :hl-symbol "#ECEFF4"
  :hl-package "#88C0D0"
  :hl-special "#B48EAD"
  :hl-paren "#4C566A"
  :hl-paren-match-bg "#434C5E"
  :hl-paren-mismatch-bg "#BF616A"
  :prompt-primary "#88C0D0"
  :prompt-package "#616E88"
  :error "#BF616A"
  :warning "#D08770"
  :info "#88C0D0"
  :dim "#616E88"))

(register-browser-theme
 (make-browser-theme
  :name :nord
  :display-name "Nord"
  :dark-p t
  :bg-primary "#2e3440"
  :bg-secondary "#3b4252"
  :bg-tertiary "#434c5e"
  :fg-primary "#eceff4"
  :fg-secondary "#e5e9f0"
  :fg-muted "#4c566a"
  :accent "#88c0d0"
  :accent-hover "#81a1c1"
  :border "#434c5e"
  :syntax-keyword "#b48ead"
  :syntax-string "#a3be8c"
  :syntax-comment "#616e88"
  :syntax-number "#d08770"
  :syntax-symbol "#eceff4"
  :syntax-package "#88c0d0"
  :syntax-special "#b48ead"
  :syntax-error "#bf616a"
  :ansi-black "#3b4252"
  :ansi-red "#bf616a"
  :ansi-green "#a3be8c"
  :ansi-yellow "#ebcb8b"
  :ansi-blue "#81a1c1"
  :ansi-magenta "#b48ead"
  :ansi-cyan "#88c0d0"
  :ansi-white "#e5e9f0"
  :ansi-bright-black "#4c566a"
  :ansi-bright-red "#bf616a"
  :ansi-bright-green "#a3be8c"
  :ansi-bright-yellow "#ebcb8b"
  :ansi-bright-blue "#81a1c1"
  :ansi-bright-magenta "#b48ead"
  :ansi-bright-cyan "#8fbcbb"
  :ansi-bright-white "#eceff4"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: ONE DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :one-dark
  :display-name "One Dark"
  :dark-p t
  :hl-keyword "#C678DD"
  :hl-string "#98C379"
  :hl-comment "#5C6370"
  :hl-number "#D19A66"
  :hl-symbol "#ABB2BF"
  :hl-package "#56B6C2"
  :hl-special "#C678DD"
  :hl-paren "#5C6370"
  :hl-paren-match-bg "#2C313A"
  :hl-paren-mismatch-bg "#E06C75"
  :prompt-primary "#56B6C2"
  :prompt-package "#5C6370"
  :error "#E06C75"
  :warning "#D19A66"
  :info "#56B6C2"
  :dim "#5C6370"))

(register-browser-theme
 (make-browser-theme
  :name :one-dark
  :display-name "One Dark"
  :dark-p t
  :bg-primary "#282c34"
  :bg-secondary "#21252b"
  :bg-tertiary "#2c313a"
  :fg-primary "#abb2bf"
  :fg-secondary "#abb2bf"
  :fg-muted "#5c6370"
  :accent "#61afef"
  :accent-hover "#528bff"
  :border "#181a1f"
  :syntax-keyword "#c678dd"
  :syntax-string "#98c379"
  :syntax-comment "#5c6370"
  :syntax-number "#d19a66"
  :syntax-symbol "#abb2bf"
  :syntax-package "#56b6c2"
  :syntax-special "#c678dd"
  :syntax-error "#e06c75"
  :ansi-black "#282c34"
  :ansi-red "#e06c75"
  :ansi-green "#98c379"
  :ansi-yellow "#e5c07b"
  :ansi-blue "#61afef"
  :ansi-magenta "#c678dd"
  :ansi-cyan "#56b6c2"
  :ansi-white "#abb2bf"
  :ansi-bright-black "#5c6370"
  :ansi-bright-red "#e06c75"
  :ansi-bright-green "#98c379"
  :ansi-bright-yellow "#e5c07b"
  :ansi-bright-blue "#61afef"
  :ansi-bright-magenta "#c678dd"
  :ansi-bright-cyan "#56b6c2"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GRUVBOX DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :gruvbox-dark
  :display-name "Gruvbox Dark"
  :dark-p t
  :hl-keyword "#D3869B"
  :hl-string "#B8BB26"
  :hl-comment "#928374"
  :hl-number "#FE8019"
  :hl-symbol "#EBDBB2"
  :hl-package "#83A598"
  :hl-special "#D3869B"
  :hl-paren "#A89984"
  :hl-paren-match-bg "#504945"
  :hl-paren-mismatch-bg "#FB4934"
  :prompt-primary "#FE8019"
  :prompt-package "#928374"
  :error "#FB4934"
  :warning "#FE8019"
  :info "#83A598"
  :dim "#928374"))

(register-browser-theme
 (make-browser-theme
  :name :gruvbox-dark
  :display-name "Gruvbox Dark"
  :dark-p t
  :bg-primary "#282828"
  :bg-secondary "#3c3836"
  :bg-tertiary "#504945"
  :fg-primary "#ebdbb2"
  :fg-secondary "#d5c4a1"
  :fg-muted "#928374"
  :accent "#fe8019"
  :accent-hover "#fabd2f"
  :border "#504945"
  :syntax-keyword "#d3869b"
  :syntax-string "#b8bb26"
  :syntax-comment "#928374"
  :syntax-number "#fe8019"
  :syntax-symbol "#ebdbb2"
  :syntax-package "#83a598"
  :syntax-special "#d3869b"
  :syntax-error "#fb4934"
  :ansi-black "#282828"
  :ansi-red "#cc241d"
  :ansi-green "#98971a"
  :ansi-yellow "#d79921"
  :ansi-blue "#458588"
  :ansi-magenta "#b16286"
  :ansi-cyan "#689d6a"
  :ansi-white "#a89984"
  :ansi-bright-black "#928374"
  :ansi-bright-red "#fb4934"
  :ansi-bright-green "#b8bb26"
  :ansi-bright-yellow "#fabd2f"
  :ansi-bright-blue "#83a598"
  :ansi-bright-magenta "#d3869b"
  :ansi-bright-cyan "#8ec07c"
  :ansi-bright-white "#ebdbb2"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: TOKYO NIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :tokyo-night
  :display-name "Tokyo Night"
  :dark-p t
  :hl-keyword "#BB9AF7"
  :hl-string "#9ECE6A"
  :hl-comment "#565F89"
  :hl-number "#FF9E64"
  :hl-symbol "#C0CAF5"
  :hl-package "#7DCFFF"
  :hl-special "#BB9AF7"
  :hl-paren "#414868"
  :hl-paren-match-bg "#24283B"
  :hl-paren-mismatch-bg "#F7768E"
  :prompt-primary "#7AA2F7"
  :prompt-package "#565F89"
  :error "#F7768E"
  :warning "#FF9E64"
  :info "#7DCFFF"
  :dim "#565F89"))

(register-browser-theme
 (make-browser-theme
  :name :tokyo-night
  :display-name "Tokyo Night"
  :dark-p t
  :bg-primary "#1a1b26"
  :bg-secondary "#24283b"
  :bg-tertiary "#414868"
  :fg-primary "#c0caf5"
  :fg-secondary "#a9b1d6"
  :fg-muted "#565f89"
  :accent "#7aa2f7"
  :accent-hover "#bb9af7"
  :border "#414868"
  :syntax-keyword "#bb9af7"
  :syntax-string "#9ece6a"
  :syntax-comment "#565f89"
  :syntax-number "#ff9e64"
  :syntax-symbol "#c0caf5"
  :syntax-package "#7dcfff"
  :syntax-special "#bb9af7"
  :syntax-error "#f7768e"
  :ansi-black "#15161e"
  :ansi-red "#f7768e"
  :ansi-green "#9ece6a"
  :ansi-yellow "#e0af68"
  :ansi-blue "#7aa2f7"
  :ansi-magenta "#bb9af7"
  :ansi-cyan "#7dcfff"
  :ansi-white "#a9b1d6"
  :ansi-bright-black "#414868"
  :ansi-bright-red "#f7768e"
  :ansi-bright-green "#9ece6a"
  :ansi-bright-yellow "#e0af68"
  :ansi-bright-blue "#7aa2f7"
  :ansi-bright-magenta "#bb9af7"
  :ansi-bright-cyan "#7dcfff"
  :ansi-bright-white "#c0caf5"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: CATPPUCCIN MOCHA
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :catppuccin-mocha
  :display-name "Catppuccin Mocha"
  :dark-p t
  :hl-keyword "#CBA6F7"
  :hl-string "#A6E3A1"
  :hl-comment "#6C7086"
  :hl-number "#FAB387"
  :hl-symbol "#CDD6F4"
  :hl-package "#89DCEB"
  :hl-special "#F5C2E7"
  :hl-paren "#585B70"
  :hl-paren-match-bg "#313244"
  :hl-paren-mismatch-bg "#F38BA8"
  :prompt-primary "#89B4FA"
  :prompt-package "#6C7086"
  :error "#F38BA8"
  :warning "#FAB387"
  :info "#89DCEB"
  :dim "#6C7086"))

(register-browser-theme
 (make-browser-theme
  :name :catppuccin-mocha
  :display-name "Catppuccin Mocha"
  :dark-p t
  :bg-primary "#1e1e2e"
  :bg-secondary "#313244"
  :bg-tertiary "#45475a"
  :fg-primary "#cdd6f4"
  :fg-secondary "#bac2de"
  :fg-muted "#6c7086"
  :accent "#89b4fa"
  :accent-hover "#b4befe"
  :border "#45475a"
  :syntax-keyword "#cba6f7"
  :syntax-string "#a6e3a1"
  :syntax-comment "#6c7086"
  :syntax-number "#fab387"
  :syntax-symbol "#cdd6f4"
  :syntax-package "#89dceb"
  :syntax-special "#f5c2e7"
  :syntax-error "#f38ba8"
  :ansi-black "#45475a"
  :ansi-red "#f38ba8"
  :ansi-green "#a6e3a1"
  :ansi-yellow "#f9e2af"
  :ansi-blue "#89b4fa"
  :ansi-magenta "#cba6f7"
  :ansi-cyan "#89dceb"
  :ansi-white "#bac2de"
  :ansi-bright-black "#585b70"
  :ansi-bright-red "#f38ba8"
  :ansi-bright-green "#a6e3a1"
  :ansi-bright-yellow "#f9e2af"
  :ansi-bright-blue "#89b4fa"
  :ansi-bright-magenta "#cba6f7"
  :ansi-bright-cyan "#89dceb"
  :ansi-bright-white "#a6adc8"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: MONOKAI
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :monokai
  :display-name "Monokai"
  :dark-p t
  :hl-keyword "#F92672"
  :hl-string "#E6DB74"
  :hl-comment "#75715E"
  :hl-number "#AE81FF"
  :hl-symbol "#F8F8F2"
  :hl-package "#66D9EF"
  :hl-special "#F92672"
  :hl-paren "#75715E"
  :hl-paren-match-bg "#49483E"
  :hl-paren-mismatch-bg "#F92672"
  :prompt-primary "#A6E22E"
  :prompt-package "#75715E"
  :error "#F92672"
  :warning "#E6DB74"
  :info "#66D9EF"
  :dim "#75715E"))

(register-browser-theme
 (make-browser-theme
  :name :monokai
  :display-name "Monokai"
  :dark-p t
  :bg-primary "#272822"
  :bg-secondary "#3e3d32"
  :bg-tertiary "#49483e"
  :fg-primary "#f8f8f2"
  :fg-secondary "#f8f8f2"
  :fg-muted "#75715e"
  :accent "#a6e22e"
  :accent-hover "#f92672"
  :border "#49483e"
  :syntax-keyword "#f92672"
  :syntax-string "#e6db74"
  :syntax-comment "#75715e"
  :syntax-number "#ae81ff"
  :syntax-symbol "#f8f8f2"
  :syntax-package "#66d9ef"
  :syntax-special "#f92672"
  :syntax-error "#f92672"
  :ansi-black "#272822"
  :ansi-red "#f92672"
  :ansi-green "#a6e22e"
  :ansi-yellow "#f4bf75"
  :ansi-blue "#66d9ef"
  :ansi-magenta "#ae81ff"
  :ansi-cyan "#a1efe4"
  :ansi-white "#f8f8f2"
  :ansi-bright-black "#75715e"
  :ansi-bright-red "#f92672"
  :ansi-bright-green "#a6e22e"
  :ansi-bright-yellow "#f4bf75"
  :ansi-bright-blue "#66d9ef"
  :ansi-bright-magenta "#ae81ff"
  :ansi-bright-cyan "#a1efe4"
  :ansi-bright-white "#f9f8f5"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: SOLARIZED DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :solarized-dark
  :display-name "Solarized Dark"
  :dark-p t
  :hl-keyword "#B58900"
  :hl-string "#2AA198"
  :hl-comment "#586E75"
  :hl-number "#CB4B16"
  :hl-symbol "#839496"
  :hl-package "#268BD2"
  :hl-special "#D33682"
  :hl-paren "#657B83"
  :hl-paren-match-bg "#073642"
  :hl-paren-mismatch-bg "#DC322F"
  :prompt-primary "#268BD2"
  :prompt-package "#586E75"
  :error "#DC322F"
  :warning "#B58900"
  :info "#2AA198"
  :dim "#586E75"))

(register-browser-theme
 (make-browser-theme
  :name :solarized-dark
  :display-name "Solarized Dark"
  :dark-p t
  :bg-primary "#002b36"
  :bg-secondary "#073642"
  :bg-tertiary "#586e75"
  :fg-primary "#839496"
  :fg-secondary "#93a1a1"
  :fg-muted "#586e75"
  :accent "#268bd2"
  :accent-hover "#2aa198"
  :border "#073642"
  :syntax-keyword "#b58900"
  :syntax-string "#2aa198"
  :syntax-comment "#586e75"
  :syntax-number "#cb4b16"
  :syntax-symbol "#839496"
  :syntax-package "#268bd2"
  :syntax-special "#d33682"
  :syntax-error "#dc322f"
  :ansi-black "#073642"
  :ansi-red "#dc322f"
  :ansi-green "#859900"
  :ansi-yellow "#b58900"
  :ansi-blue "#268bd2"
  :ansi-magenta "#d33682"
  :ansi-cyan "#2aa198"
  :ansi-white "#eee8d5"
  :ansi-bright-black "#002b36"
  :ansi-bright-red "#cb4b16"
  :ansi-bright-green "#586e75"
  :ansi-bright-yellow "#657b83"
  :ansi-bright-blue "#839496"
  :ansi-bright-magenta "#6c71c4"
  :ansi-bright-cyan "#93a1a1"
  :ansi-bright-white "#fdf6e3"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GITHUB LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :github-light
  :display-name "GitHub Light"
  :dark-p nil
  :hl-keyword "#D73A49"
  :hl-string "#032F62"
  :hl-comment "#6A737D"
  :hl-number "#005CC5"
  :hl-symbol "#24292E"
  :hl-package "#6F42C1"
  :hl-special "#D73A49"
  :hl-paren "#959DA5"
  :hl-paren-match-bg "#E1E4E8"
  :hl-paren-mismatch-bg "#FDAEB7"
  :prompt-primary "#0366D6"
  :prompt-package "#6A737D"
  :error "#CB2431"
  :warning "#B08800"
  :info "#0366D6"
  :dim "#6A737D"))

(register-browser-theme
 (make-browser-theme
  :name :github-light
  :display-name "GitHub Light"
  :dark-p nil
  :bg-primary "#ffffff"
  :bg-secondary "#f6f8fa"
  :bg-tertiary "#e1e4e8"
  :fg-primary "#24292e"
  :fg-secondary "#586069"
  :fg-muted "#6a737d"
  :accent "#0366d6"
  :accent-hover "#0056b3"
  :border "#e1e4e8"
  :syntax-keyword "#d73a49"
  :syntax-string "#032f62"
  :syntax-comment "#6a737d"
  :syntax-number "#005cc5"
  :syntax-symbol "#24292e"
  :syntax-package "#6f42c1"
  :syntax-special "#d73a49"
  :syntax-error "#cb2431"
  :ansi-black "#24292e"
  :ansi-red "#d73a49"
  :ansi-green "#22863a"
  :ansi-yellow "#b08800"
  :ansi-blue "#0366d6"
  :ansi-magenta "#6f42c1"
  :ansi-cyan "#1b7c83"
  :ansi-white "#fafbfc"
  :ansi-bright-black "#586069"
  :ansi-bright-red "#cb2431"
  :ansi-bright-green "#28a745"
  :ansi-bright-yellow "#dbab09"
  :ansi-bright-blue "#2188ff"
  :ansi-bright-magenta "#8a63d2"
  :ansi-bright-cyan "#3192aa"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: SOLARIZED LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :solarized-light
  :display-name "Solarized Light"
  :dark-p nil
  :hl-keyword "#B58900"
  :hl-string "#2AA198"
  :hl-comment "#93A1A1"
  :hl-number "#CB4B16"
  :hl-symbol "#657B83"
  :hl-package "#268BD2"
  :hl-special "#D33682"
  :hl-paren "#93A1A1"
  :hl-paren-match-bg "#EEE8D5"
  :hl-paren-mismatch-bg "#DC322F"
  :prompt-primary "#268BD2"
  :prompt-package "#93A1A1"
  :error "#DC322F"
  :warning "#B58900"
  :info "#2AA198"
  :dim "#93A1A1"))

(register-browser-theme
 (make-browser-theme
  :name :solarized-light
  :display-name "Solarized Light"
  :dark-p nil
  :bg-primary "#fdf6e3"
  :bg-secondary "#eee8d5"
  :bg-tertiary "#93a1a1"
  :fg-primary "#657b83"
  :fg-secondary "#586e75"
  :fg-muted "#93a1a1"
  :accent "#268bd2"
  :accent-hover "#2aa198"
  :border "#eee8d5"
  :syntax-keyword "#b58900"
  :syntax-string "#2aa198"
  :syntax-comment "#93a1a1"
  :syntax-number "#cb4b16"
  :syntax-symbol "#657b83"
  :syntax-package "#268bd2"
  :syntax-special "#d33682"
  :syntax-error "#dc322f"
  :ansi-black "#073642"
  :ansi-red "#dc322f"
  :ansi-green "#859900"
  :ansi-yellow "#b58900"
  :ansi-blue "#268bd2"
  :ansi-magenta "#d33682"
  :ansi-cyan "#2aa198"
  :ansi-white "#eee8d5"
  :ansi-bright-black "#002b36"
  :ansi-bright-red "#cb4b16"
  :ansi-bright-green "#586e75"
  :ansi-bright-yellow "#657b83"
  :ansi-bright-blue "#839496"
  :ansi-bright-magenta "#6c71c4"
  :ansi-bright-cyan "#93a1a1"
  :ansi-bright-white "#fdf6e3"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GRUVBOX LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :gruvbox-light
  :display-name "Gruvbox Light"
  :dark-p nil
  :hl-keyword "#8F3F71"
  :hl-string "#79740E"
  :hl-comment "#928374"
  :hl-number "#D65D0E"
  :hl-symbol "#3C3836"
  :hl-package "#076678"
  :hl-special "#8F3F71"
  :hl-paren "#A89984"
  :hl-paren-match-bg "#EBDBB2"
  :hl-paren-mismatch-bg "#9D0006"
  :prompt-primary "#D65D0E"
  :prompt-package "#928374"
  :error "#9D0006"
  :warning "#D65D0E"
  :info "#076678"
  :dim "#928374"))

(register-browser-theme
 (make-browser-theme
  :name :gruvbox-light
  :display-name "Gruvbox Light"
  :dark-p nil
  :bg-primary "#fbf1c7"
  :bg-secondary "#ebdbb2"
  :bg-tertiary "#d5c4a1"
  :fg-primary "#3c3836"
  :fg-secondary "#504945"
  :fg-muted "#928374"
  :accent "#d65d0e"
  :accent-hover "#af3a03"
  :border "#d5c4a1"
  :syntax-keyword "#8f3f71"
  :syntax-string "#79740e"
  :syntax-comment "#928374"
  :syntax-number "#d65d0e"
  :syntax-symbol "#3c3836"
  :syntax-package "#076678"
  :syntax-special "#8f3f71"
  :syntax-error "#9d0006"
  :ansi-black "#fbf1c7"
  :ansi-red "#cc241d"
  :ansi-green "#98971a"
  :ansi-yellow "#d79921"
  :ansi-blue "#458588"
  :ansi-magenta "#b16286"
  :ansi-cyan "#689d6a"
  :ansi-white "#7c6f64"
  :ansi-bright-black "#928374"
  :ansi-bright-red "#9d0006"
  :ansi-bright-green "#79740e"
  :ansi-bright-yellow "#b57614"
  :ansi-bright-blue "#076678"
  :ansi-bright-magenta "#8f3f71"
  :ansi-bright-cyan "#427b58"
  :ansi-bright-white "#3c3836"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: ONE LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :one-light
  :display-name "One Light"
  :dark-p nil
  :hl-keyword "#A626A4"
  :hl-string "#50A14F"
  :hl-comment "#A0A1A7"
  :hl-number "#986801"
  :hl-symbol "#383A42"
  :hl-package "#0184BC"
  :hl-special "#A626A4"
  :hl-paren "#A0A1A7"
  :hl-paren-match-bg "#EAEAEB"
  :hl-paren-mismatch-bg "#E45649"
  :prompt-primary "#0184BC"
  :prompt-package "#A0A1A7"
  :error "#E45649"
  :warning "#986801"
  :info "#0184BC"
  :dim "#A0A1A7"))

(register-browser-theme
 (make-browser-theme
  :name :one-light
  :display-name "One Light"
  :dark-p nil
  :bg-primary "#fafafa"
  :bg-secondary "#eaeaeb"
  :bg-tertiary "#dbdbdc"
  :fg-primary "#383a42"
  :fg-secondary "#4b4e55"
  :fg-muted "#a0a1a7"
  :accent "#4078f2"
  :accent-hover "#526fff"
  :border "#dbdbdc"
  :syntax-keyword "#a626a4"
  :syntax-string "#50a14f"
  :syntax-comment "#a0a1a7"
  :syntax-number "#986801"
  :syntax-symbol "#383a42"
  :syntax-package "#0184bc"
  :syntax-special "#a626a4"
  :syntax-error "#e45649"
  :ansi-black "#383a42"
  :ansi-red "#e45649"
  :ansi-green "#50a14f"
  :ansi-yellow "#c18401"
  :ansi-blue "#4078f2"
  :ansi-magenta "#a626a4"
  :ansi-cyan "#0184bc"
  :ansi-white "#fafafa"
  :ansi-bright-black "#a0a1a7"
  :ansi-bright-red "#e45649"
  :ansi-bright-green "#50a14f"
  :ansi-bright-yellow "#c18401"
  :ansi-bright-blue "#4078f2"
  :ansi-bright-magenta "#a626a4"
  :ansi-bright-cyan "#0184bc"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; User-facing Theme Functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun set-terminal-theme (name)
  "Set the terminal theme by name (keyword or string)."
  (when (stringp name)
    (setf name (intern (string-upcase name) :keyword)))
  (apply-terminal-theme name))

(defun set-browser-theme (name)
  "Set the browser theme by name (keyword or string)."
  (when (stringp name)
    (setf name (intern (string-upcase name) :keyword)))
  (apply-browser-theme name))

(defun current-terminal-theme-name ()
  "Get the name of the current terminal theme."
  (when *current-terminal-theme*
    (terminal-theme-name *current-terminal-theme*)))

(defun current-browser-theme-name ()
  "Get the name of the current browser theme."
  (when *current-browser-theme*
    (browser-theme-name *current-browser-theme*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Markdown Style from Theme
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *current-markdown-style* nil
  "Cached markdown style derived from the current terminal theme.")

(defun current-markdown-style ()
  "Get a markdown style matching the current terminal theme.
   Caches the result in *current-markdown-style*."
  (or *current-markdown-style*
      (if *current-terminal-theme*
          (let ((theme *current-terminal-theme*))
            (setf *current-markdown-style*
                  (tuition:make-markdown-style-from-colors
                   :accent (%ensure-hex (terminal-theme-info theme))
                   :secondary (%ensure-hex (terminal-theme-hl-special theme))
                   :string (%ensure-hex (terminal-theme-hl-string theme))
                   :comment (%ensure-hex (terminal-theme-hl-comment theme))
                   :muted (%ensure-hex (terminal-theme-dim theme)))))
          (tuition:make-style-dark))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Initialization
;;; ─────────────────────────────────────────────────────────────────────────────

(defun initialize-themes ()
  "Initialize the theming system with auto-detection."
  ;; Auto-select terminal theme based on dark mode
  (auto-select-terminal-theme)
  ;; Auto-select browser theme based on same detection
  ;; (will be overridden when client reports its own preference)
  (auto-select-browser-theme))
