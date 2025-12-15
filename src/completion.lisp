;;; completion.lisp --- Tab completion for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Completion Menu State
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct completion-menu
  "State for the dropdown completion menu."
  (active nil :type boolean)           ; Is menu currently displayed?
  (candidates nil :type list)          ; List of completion strings
  (selected 0 :type fixnum)            ; Currently selected index
  (scroll-offset 0 :type fixnum)       ; Scroll position for long lists
  (prefix "" :type string)             ; Original prefix being completed
  (start-col 0 :type fixnum)           ; Column where prefix starts
  (max-visible 10 :type fixnum)        ; Max items to show at once
  (render-above nil :type boolean))    ; T if menu rendered above cursor

(defvar *completion-menu* (make-completion-menu)
  "Global completion menu state.")

(defun reset-completion-menu ()
  "Reset the completion menu to inactive state."
  (setf (completion-menu-active *completion-menu*) nil
        (completion-menu-candidates *completion-menu*) nil
        (completion-menu-selected *completion-menu*) 0
        (completion-menu-scroll-offset *completion-menu*) 0
        (completion-menu-prefix *completion-menu*) ""
        (completion-menu-start-col *completion-menu*) 0
        (completion-menu-render-above *completion-menu*) nil))

(defun completion-menu-active-p ()
  "Return T if completion menu is currently active."
  (completion-menu-active *completion-menu*))

(defun completion-menu-count ()
  "Return number of candidates in menu."
  (length (completion-menu-candidates *completion-menu*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Completion Menu Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun menu-select-next ()
  "Move selection to next item, with wrapping."
  (let* ((menu *completion-menu*)
         (count (length (completion-menu-candidates menu)))
         (new-selected (mod (1+ (completion-menu-selected menu)) count))
         (max-visible (completion-menu-max-visible menu))
         (scroll (completion-menu-scroll-offset menu)))
    (setf (completion-menu-selected menu) new-selected)
    ;; Adjust scroll if needed
    (when (>= new-selected (+ scroll max-visible))
      (setf (completion-menu-scroll-offset menu)
            (- new-selected max-visible -1)))
    (when (< new-selected scroll)
      (setf (completion-menu-scroll-offset menu) new-selected))))

(defun menu-select-previous ()
  "Move selection to previous item, with wrapping."
  (let* ((menu *completion-menu*)
         (count (length (completion-menu-candidates menu)))
         (new-selected (mod (1- (completion-menu-selected menu)) count))
         (scroll (completion-menu-scroll-offset menu)))
    (setf (completion-menu-selected menu) new-selected)
    ;; Adjust scroll if needed
    (when (< new-selected scroll)
      (setf (completion-menu-scroll-offset menu) new-selected))
    (when (>= new-selected (+ scroll (completion-menu-max-visible menu)))
      (setf (completion-menu-scroll-offset menu)
            (- new-selected (completion-menu-max-visible menu) -1)))))

(defun menu-get-selected ()
  "Return the currently selected completion string."
  (let ((menu *completion-menu*))
    (nth (completion-menu-selected menu)
         (completion-menu-candidates menu))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Legacy Completion State (for compatibility)
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *completion-candidates* nil
  "Current list of completion candidates.")

(defvar *completion-index* 0
  "Current index in completion candidates for cycling.")

(defvar *completion-prefix* nil
  "The prefix being completed.")

(defvar *completion-start* 0
  "Start position of the prefix in the line.")

(defun reset-completion-state ()
  "Reset completion state for new completion."
  (setf *completion-candidates* nil
        *completion-index* 0
        *completion-prefix* nil
        *completion-start* 0)
  (reset-completion-menu))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Word Extraction
;;; ─────────────────────────────────────────────────────────────────────────────

(defun completion-word-char-p (char)
  "Return T if CHAR can be part of a completion word."
  (or (alphanumericp char)
      (member char '(#\- #\_ #\* #\+ #\/ #\= #\< #\> #\! #\? #\% #\& #\$
                     #\: #\. #\~ #\^ #\,))))

(defun extract-completion-prefix (line col)
  "Extract the word to complete from LINE ending at COL.
   Returns (values prefix start-col type) where type is :symbol, :package, :qualified, :keyword, :path, or :command."
  (when (zerop col)
    (return-from extract-completion-prefix (values "" 0 :symbol)))
  (let ((start col))
    ;; Scan backwards to find start of word
    (loop while (and (plusp start)
                     (completion-word-char-p (char line (1- start))))
          do (decf start))
    (let ((prefix (subseq line start col)))
      (values prefix start (classify-prefix-with-context prefix line start)))))

(defun classify-prefix (prefix)
  "Classify PREFIX to determine completion type."
  (cond
    ;; Empty
    ((zerop (length prefix)) :symbol)
    ;; Command (starts with comma)
    ((char= (char prefix 0) #\,) :command)
    ;; Keyword
    ((char= (char prefix 0) #\:) :keyword)
    ;; Path (starts with / or ./ or ~/ or contains /)
    ((or (char= (char prefix 0) #\/)
         (char= (char prefix 0) #\~)
         (and (>= (length prefix) 2)
              (char= (char prefix 0) #\.)
              (char= (char prefix 1) #\/))
         (and (>= (length prefix) 2)
              (char= (char prefix 0) #\#)
              (char= (char prefix 1) #\p)))
     :path)
    ;; Package-qualified (contains : or ::)
    ((position #\: prefix) :qualified)
    ;; Regular symbol
    (t :symbol)))

(defun command-context-type (line)
  "Determine completion type based on command context in LINE.
   Returns NIL if no special context, or :package, :path, etc."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (cond
      ;; Package commands: ,cd, ,in-package
      ((or (string-prefix-p ",cd " trimmed)
           (string-prefix-p ",in-package " trimmed))
       :package)
      ;; File commands: ,load, ,ld, ,compile-file, ,cf
      ((or (string-prefix-p ",load " trimmed)
           (string-prefix-p ",ld " trimmed)
           (string-prefix-p ",compile-file " trimmed)
           (string-prefix-p ",cf " trimmed))
       :path)
      ;; System loading: ,load-system, ,ql
      ((or (string-prefix-p ",load-system " trimmed)
           (string-prefix-p ",ql " trimmed))
       :system)
      (t nil))))

(defun string-prefix-p (prefix string)
  "Return T if STRING starts with PREFIX (case-insensitive)."
  (and (>= (length string) (length prefix))
       (string-equal prefix (subseq string 0 (length prefix)))))

(defun classify-prefix-with-context (prefix line start)
  "Classify PREFIX considering the LINE context.
   START is where PREFIX begins in LINE."
  (declare (ignore start))
  ;; Check command context first
  (let ((context-type (command-context-type line)))
    (cond
      ;; Command context overrides for non-command prefixes
      ((and context-type (not (and (plusp (length prefix))
                                   (char= (char prefix 0) #\,))))
       context-type)
      ;; Otherwise use standard classification
      (t (classify-prefix prefix)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Symbol Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-symbol (prefix &optional (package *icl-package*))
  "Return list of symbols in PACKAGE matching PREFIX."
  (let ((upprefix (string-upcase prefix))
        (results nil))
    ;; Accessible symbols (external from used packages + internal)
    (do-symbols (sym package)
      (when (prefix-match-p upprefix (symbol-name sym))
        (push (format-symbol-for-completion sym package) results)))
    ;; Sort alphabetically
    (sort results #'string<)))

(defun complete-external-symbol (prefix package)
  "Return list of external symbols in PACKAGE matching PREFIX."
  (let ((upprefix (string-upcase prefix))
        (results nil))
    (do-external-symbols (sym package)
      (when (prefix-match-p upprefix (symbol-name sym))
        (push (string-downcase (symbol-name sym)) results)))
    (sort results #'string<)))

(defun format-symbol-for-completion (sym package)
  "Format SYM for completion display relative to PACKAGE."
  (let ((name (symbol-name sym))
        (home (symbol-package sym)))
    (cond
      ;; Symbol is in the completion package
      ((eql home package) (string-downcase name))
      ;; Symbol is external in its home package and accessible
      ((and home (symbol-external-p sym home))
       (string-downcase name))
      ;; Otherwise just the name
      (t (string-downcase name)))))

(defun symbol-external-p (sym package)
  "Return T if SYM is external in PACKAGE."
  (multiple-value-bind (s status) (find-symbol (symbol-name sym) package)
    (declare (ignore s))
    (eql status :external)))

(defun prefix-match-p (prefix string)
  "Return T if PREFIX is a prefix of STRING (case-insensitive)."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Keyword Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-keyword (prefix)
  "Return list of keywords matching PREFIX (including leading colon)."
  (let* ((key-prefix (if (and (plusp (length prefix))
                              (char= (char prefix 0) #\:))
                         (subseq prefix 1)
                         prefix))
         (upprefix (string-upcase key-prefix))
         (results nil))
    (do-symbols (sym (find-package :keyword))
      (when (prefix-match-p upprefix (symbol-name sym))
        (push (format nil ":~A" (string-downcase (symbol-name sym))) results)))
    (sort results #'string<)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Package Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-package (prefix)
  "Return list of package names matching PREFIX."
  (let ((upprefix (string-upcase prefix))
        (results nil))
    (dolist (pkg (list-all-packages))
      (let ((name (package-name pkg)))
        (when (prefix-match-p upprefix name)
          (push (string-downcase name) results)))
      ;; Also check nicknames
      (dolist (nick (package-nicknames pkg))
        (when (prefix-match-p upprefix nick)
          (push (string-downcase nick) results))))
    (sort (remove-duplicates results :test #'string=) #'string<)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Package-Qualified Symbol Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-qualified (prefix)
  "Complete package:symbol or package::symbol.
   Returns list of completions."
  (let* ((double-colon-pos (search "::" prefix))
         (single-colon-pos (position #\: prefix))
         (internal-p double-colon-pos)
         (colon-pos (or double-colon-pos single-colon-pos)))
    (unless colon-pos
      (return-from complete-qualified nil))
    (let* ((pkg-name (subseq prefix 0 colon-pos))
           (sym-prefix (subseq prefix (if internal-p
                                          (+ colon-pos 2)
                                          (1+ colon-pos))))
           (pkg (find-package (string-upcase pkg-name))))
      (unless pkg
        ;; Package doesn't exist - complete package names instead
        (return-from complete-qualified
          (mapcar (lambda (p) (concatenate 'string p ":"))
                  (complete-package pkg-name))))
      ;; Complete symbols in package
      (let ((separator (if internal-p "::" ":"))
            (pkg-prefix (string-downcase pkg-name)))
        (if internal-p
            ;; Internal - all symbols
            (mapcar (lambda (s) (concatenate 'string pkg-prefix separator s))
                    (complete-symbol sym-prefix pkg))
            ;; External only
            (mapcar (lambda (s) (concatenate 'string pkg-prefix separator s))
                    (complete-external-symbol sym-prefix pkg)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Path Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-path (prefix)
  "Complete file path starting with PREFIX."
  (let* ((clean-prefix (cond
                         ;; #p"path" syntax
                         ((and (>= (length prefix) 3)
                               (string= "#p\"" (subseq prefix 0 3)))
                          (subseq prefix 3))
                         ((and (>= (length prefix) 3)
                               (string= "#P\"" (subseq prefix 0 3)))
                          (subseq prefix 3))
                         ;; #p without quote
                         ((and (>= (length prefix) 2)
                               (or (string= "#p" (subseq prefix 0 2))
                                   (string= "#P" (subseq prefix 0 2))))
                          (subseq prefix 2))
                         (t prefix)))
         ;; Expand ~ to home directory
         (expanded (if (and (plusp (length clean-prefix))
                            (char= (char clean-prefix 0) #\~))
                       (concatenate 'string
                                    (namestring (user-homedir-pathname))
                                    (subseq clean-prefix 1))
                       clean-prefix))
         (dir (directory-namestring expanded))
         (name-prefix (file-namestring expanded))
         (results nil))
    (when (or (zerop (length dir))
              (probe-file dir))
      (let ((search-dir (if (zerop (length dir))
                            (make-pathname :directory '(:relative))
                            (pathname dir))))
        (handler-case
            (dolist (path (directory (merge-pathnames
                                      (make-pathname :name :wild :type :wild)
                                      search-dir)))
              (let ((name (if (pathname-name path)
                              (if (pathname-type path)
                                  (format nil "~A.~A"
                                          (pathname-name path)
                                          (pathname-type path))
                                  (pathname-name path))
                              ;; Directory
                              (first (last (pathname-directory path))))))
                (when (and name (prefix-match-p (string-upcase name-prefix)
                                                (string-upcase name)))
                  (let ((full-path (namestring path)))
                    ;; Add trailing / for directories
                    (when (and (not (pathname-name path))
                               (not (alexandria:ends-with #\/ full-path)))
                      (setf full-path (concatenate 'string full-path "/")))
                    (push full-path results)))))
          (error () nil))))
    (sort results #'string<)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main Completion Interface
;;; ─────────────────────────────────────────────────────────────────────────────

(defun compute-completions (prefix type)
  "Compute completion candidates for PREFIX of TYPE using Slynk backend."
  (case type
    (:command (complete-command prefix))
    (:package (complete-package-via-slynk prefix))
    (:system (complete-system-via-slynk prefix))
    (:symbol (complete-symbol-via-slynk prefix))
    (:keyword (complete-keyword-via-slynk prefix))
    (:qualified (complete-qualified-via-slynk prefix))
    (:path (complete-path prefix))  ; Paths are always local
    (otherwise (complete-symbol-via-slynk prefix))))

(defun complete-command (prefix)
  "Complete command PREFIX (starts with comma).
   Returns list of matching command names with comma prefix."
  (let* ((cmd-prefix (if (and (plusp (length prefix))
                              (char= (char prefix 0) #\,))
                         (subseq prefix 1)
                         prefix))
         (up-prefix (string-upcase cmd-prefix))
         (results nil))
    ;; Get all commands and filter by prefix
    (dolist (cmd (list-commands))
      (let ((name (string-downcase (symbol-name (command-name cmd)))))
        (when (prefix-match-p up-prefix (string-upcase name))
          (push (concatenate 'string "," name) results)))
      ;; Also check aliases
      (dolist (alias (command-aliases cmd))
        (let ((alias-name (string-downcase (symbol-name alias))))
          (when (prefix-match-p up-prefix (string-upcase alias-name))
            (push (concatenate 'string "," alias-name) results)))))
    (sort (remove-duplicates results :test #'string=) #'string<)))

(defun complete-package-via-slynk (prefix)
  "Complete package name PREFIX using Slynk backend."
  (handler-case
      (let* ((up-prefix (string-upcase prefix))
             (packages (slynk-list-packages))
             (results nil))
        (dolist (pkg packages)
          (when (prefix-match-p up-prefix (string-upcase pkg))
            (push pkg results)))
        (sort results #'string<))
    (error () nil)))

(defun complete-system-via-slynk (prefix)
  "Complete ASDF system name PREFIX using Slynk backend."
  (handler-case
      (let* ((up-prefix (string-upcase prefix))
             (systems (slynk-list-systems))
             (results nil))
        (dolist (sys systems)
          (when (prefix-match-p up-prefix (string-upcase sys))
            (push sys results)))
        (sort results #'string<))
    (error () nil)))

(defun slynk-list-systems ()
  "Get list of known ASDF systems via Slynk."
  (unless *slynk-connected-p*
    (return-from slynk-list-systems nil))
  (handler-case
      (slynk-client:slime-eval
       '(cl:mapcar (cl:function cl:string-downcase)
                   (asdf:registered-systems))
       *slynk-connection*)
    (error () nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Slynk-backed Completion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun complete-symbol-via-slynk (prefix)
  "Complete symbol PREFIX using Slynk backend."
  (handler-case
      (let ((result (slynk-complete-simple prefix
                                           :package (package-name *icl-package*))))
        ;; slynk-complete-simple already extracts the completions list
        (if (listp result)
            result
            nil))
    (error () nil)))

(defun complete-keyword-via-slynk (prefix)
  "Complete keyword PREFIX using Slynk backend."
  ;; Keywords start with :, which Slynk handles
  (handler-case
      (let ((result (slynk-complete-simple prefix :package "KEYWORD")))
        ;; slynk-complete-simple already extracts the completions list
        (if (listp result)
            result
            nil))
    (error ()
      ;; Fallback to local completion
      (complete-keyword prefix))))

(defun complete-qualified-via-slynk (prefix)
  "Complete package-qualified symbol PREFIX using Slynk backend."
  (handler-case
      (let ((result (slynk-complete-simple prefix
                                           :package (package-name *icl-package*))))
        ;; slynk-complete-simple already extracts the completions list
        (if (listp result)
            result
            nil))
    (error ()
      ;; Fallback to local completion
      (complete-qualified prefix))))

(defun find-common-prefix (strings)
  "Find the longest common prefix of STRINGS."
  (if (null strings)
      ""
      (let ((first (first strings)))
        (if (null (rest strings))
            first
            (let ((min-len (reduce #'min strings :key #'length)))
              (loop for i from 0 below min-len
                    for c = (char first i)
                    while (every (lambda (s) (char-equal c (char s i))) strings)
                    finally (return (subseq first 0 i))))))))

(defun attempt-completion (line col)
  "Attempt completion at position COL in LINE.
   Returns (values new-text new-col candidates) where:
   - new-text is the updated line
   - new-col is the new cursor column
   - candidates is the list of possible completions (for display)."
  (multiple-value-bind (prefix start type) (extract-completion-prefix line col)
    (let ((candidates (compute-completions prefix type)))
      (cond
        ;; No completions
        ((null candidates)
         (values line col nil))
        ;; Single completion - insert it
        ((= 1 (length candidates))
         (let* ((completion (first candidates))
                (new-line (concatenate 'string
                                       (subseq line 0 start)
                                       completion
                                       (subseq line col))))
           (values new-line (+ start (length completion)) nil)))
        ;; Multiple completions - find common prefix
        (t
         (let ((common (find-common-prefix candidates)))
           (if (> (length common) (length prefix))
               ;; Extend with common prefix
               (let ((new-line (concatenate 'string
                                            (subseq line 0 start)
                                            common
                                            (subseq line col))))
                 (values new-line (+ start (length common)) candidates))
               ;; Show candidates, no extension possible
               (values line col candidates))))))))

(defun cycle-completion (line col direction)
  "Cycle through completions in DIRECTION (:next or :prev).
   Returns (values new-text new-col) or nil if no completions active."
  (when (and *completion-candidates*
             (plusp (length *completion-candidates*)))
    (let* ((len (length *completion-candidates*))
           (new-index (mod (+ *completion-index*
                              (if (eql direction :next) 1 -1))
                           len))
           (completion (nth new-index *completion-candidates*)))
      (setf *completion-index* new-index)
      (let ((new-line (concatenate 'string
                                   (subseq line 0 *completion-start*)
                                   completion
                                   (subseq line col))))
        (values new-line (+ *completion-start* (length completion)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dropdown Menu Interface
;;; ─────────────────────────────────────────────────────────────────────────────

(defun open-completion-menu (candidates prefix start-col)
  "Open the completion menu with CANDIDATES."
  (let ((menu *completion-menu*))
    (setf (completion-menu-active menu) t
          (completion-menu-candidates menu) candidates
          (completion-menu-selected menu) 0
          (completion-menu-scroll-offset menu) 0
          (completion-menu-prefix menu) prefix
          (completion-menu-start-col menu) start-col)))

(defun close-completion-menu ()
  "Close the completion menu."
  (reset-completion-menu))

(defun apply-selected-completion (line col)
  "Apply the currently selected completion to LINE at COL.
   Returns (values new-line new-col)."
  (let* ((menu *completion-menu*)
         (selected (menu-get-selected))
         (start (completion-menu-start-col menu)))
    (when selected
      (let ((new-line (concatenate 'string
                                   (subseq line 0 start)
                                   selected
                                   (subseq line col))))
        (values new-line (+ start (length selected)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Menu Rendering (ANSI escape codes)
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *ansi-reset* (format nil "~C[0m" #\Escape))
(defvar *ansi-reverse* (format nil "~C[7m" #\Escape))
(defvar *ansi-dim* (format nil "~C[2m" #\Escape))
(defvar *ansi-bold* (format nil "~C[1m" #\Escape))
(defvar *ansi-fg-gray* (format nil "~C[38;5;245m" #\Escape))

(defun render-completion-menu (prompt-len cursor-col)
  "Render the completion dropdown menu.
   PROMPT-LEN is the length of the prompt.
   CURSOR-COL is the current cursor column in the buffer."
  (let* ((menu *completion-menu*)
         (candidates (completion-menu-candidates menu))
         (selected (completion-menu-selected menu))
         (scroll (completion-menu-scroll-offset menu))
         (max-visible (completion-menu-max-visible menu))
         (total (length candidates))
         (visible-count (min max-visible total))
         ;; Calculate menu width
         (max-width (reduce #'max candidates :key #'length :initial-value 10))
         ;; Position: at prefix start
         (menu-col (+ prompt-len (completion-menu-start-col menu) 1))
         ;; Track lines printed for cursor restoration
         (lines-printed 0))
    ;; Render menu below cursor
    (dotimes (i visible-count)
      (let* ((idx (+ scroll i))
             (item (nth idx candidates))
             (is-selected (= idx selected))
             (show-scrollbar (> total max-visible))
             (scrollbar-char (if show-scrollbar
                                 (if (scrollbar-at-position-p i scroll visible-count total)
                                     #\FULL_BLOCK
                                     #\LIGHT_SHADE)
                                 #\Space)))
        (format t "~%")
        (incf lines-printed)
        (format t "~C[~DG~C[K" #\Escape menu-col #\Escape)  ; Move to column and clear to EOL
        (if is-selected
            (format t "~A ~A~VA ~C~A"
                    *ansi-reverse* *ansi-bold*
                    max-width item scrollbar-char *ansi-reset*)
            (format t " ~VA ~A~C~A"
                    max-width item *ansi-fg-gray* scrollbar-char *ansi-reset*))))
    ;; Show count if more items
    (when (> total max-visible)
      (format t "~%")
      (incf lines-printed)
      (format t "~C[~DG~C[K" #\Escape menu-col #\Escape)  ; Move to column and clear to EOL
      (format t "~A[~D/~D]~A" *ansi-dim* (1+ selected) total *ansi-reset*))
    ;; Move cursor back up to prompt line and restore column
    (cursor-up lines-printed)
    (cursor-to-column (+ prompt-len cursor-col 1))
    (force-output)))

(defun scrollbar-at-position-p (visible-idx scroll visible-count total)
  "Return T if scrollbar thumb should be at VISIBLE-IDX."
  (let* ((thumb-size (max 1 (floor (* visible-count (/ visible-count total)))))
         (thumb-pos (floor (* (- visible-count thumb-size)
                              (/ scroll (max 1 (- total visible-count)))))))
    (and (>= visible-idx thumb-pos)
         (< visible-idx (+ thumb-pos thumb-size)))))

(defun clear-completion-menu (max-visible)
  "Clear the area where completion menu was displayed."
  (let ((lines-cleared 0))
    ;; Clear lines below
    (dotimes (i (+ max-visible 2))
      (format t "~%")
      (incf lines-cleared)
      (format t "~C[2K" #\Escape))
    ;; Move cursor back up
    (cursor-up lines-cleared)
    (force-output)))
