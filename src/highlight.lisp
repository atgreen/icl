;;; highlight.lisp --- Syntax highlighting for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *syntax-highlighting-enabled* t
  "Enable/disable syntax highlighting in the editor.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Highlight Colors (ANSI escape codes)
;;; ─────────────────────────────────────────────────────────────────────────────

;;; These variables hold ANSI escape sequences for syntax highlighting.
;;; They are regenerated from *hl-*-color* variables by refresh-highlight-colors.

(defvar *hl-string* nil)
(defvar *hl-comment* nil)
(defvar *hl-keyword* nil)
(defvar *hl-number* nil)
(defvar *hl-special* nil)
(defvar *hl-paren* nil)
(defvar *hl-quote* nil)
(defvar *hl-builtin* nil)
(defvar *hl-reset* nil)

(defun %color-with-attr (color-obj attr-prefix)
  "Resolve COLOR-OBJ to a foreground escape sequence, prepending ATTR-PREFIX if non-nil.
   ATTR-PREFIX is an SGR code like \"1;\" for bold or \"3;\" for italic."
  (let ((base (tuition:resolve-color-foreground color-obj)))
    (if (and attr-prefix base)
        ;; Insert the attribute after the ESC[ prefix
        (concatenate 'string
                     (subseq base 0 2)  ; "ESC["
                     attr-prefix
                     (subseq base 2))   ; "38;5;Nm" or "38;2;R;G;Bm"
        base)))

(defun refresh-highlight-colors ()
  "Regenerate ANSI escape sequences from complete-color objects.
   Called by the theme system when a theme is applied."
  (setf *hl-string* (tuition:resolve-color-foreground *hl-string-color*)
        *hl-comment* (%color-with-attr *hl-comment-color*
                                       (when *comment-italic* "3;"))
        *hl-keyword* (%color-with-attr *hl-keyword-color*
                                       (when *keyword-bold* "1;"))
        *hl-number* (tuition:resolve-color-foreground *hl-number-color*)
        *hl-special* (%color-with-attr *hl-special-color*
                                       (when *special-bold* "1;"))
        *hl-paren* (tuition:resolve-color-foreground *hl-paren-color*)
        *hl-quote* (tuition:resolve-color-foreground *hl-number-color*)  ; Same as number
        *hl-builtin* (tuition:resolve-color-foreground *hl-package-color*)
        *hl-reset* *ansi-reset*))

;; Fallback paren-match colors (used when theme doesn't specify)
(defvar *hl-paren-match-dark-fallback*
  (tuition:make-complete-color :truecolor "#4C4C4C"))
(defvar *hl-paren-mismatch-dark-fallback*
  (tuition:make-complete-color :truecolor "#5F0000"))
(defvar *hl-paren-match-light-fallback*
  (tuition:make-complete-color :truecolor "#DADADA"))
(defvar *hl-paren-mismatch-light-fallback*
  (tuition:make-complete-color :truecolor "#FFAFAF"))

;; Resolved paren match escape sequences
(defvar *hl-paren-match* nil)
(defvar *hl-paren-mismatch* nil)

(defun setup-highlight-colors ()
  "Set up highlight colors based on terminal theme and background.
   Uses theme-specific paren-match colors if available, else dark/light fallbacks."
  (let ((match-color (or *hl-paren-match-bg-color*
                         (if (terminal-light-p)
                             *hl-paren-match-light-fallback*
                             *hl-paren-match-dark-fallback*)))
        (mismatch-color (or *hl-paren-mismatch-bg-color*
                            (if (terminal-light-p)
                                *hl-paren-mismatch-light-fallback*
                                *hl-paren-mismatch-dark-fallback*))))
    (setf *hl-paren-match* (tuition:resolve-color-background match-color)
          *hl-paren-mismatch* (tuition:resolve-color-background mismatch-color))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Special Forms and Common Macros
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *special-forms*
  '("defun" "defmacro" "defvar" "defparameter" "defconstant" "defclass"
    "defmethod" "defgeneric" "defstruct" "defpackage" "deftype"
    "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet"
    "lambda" "function"
    "if" "when" "unless" "cond" "case" "ecase" "ccase" "typecase" "etypecase"
    "block" "return" "return-from" "tagbody" "go"
    "catch" "throw" "unwind-protect"
    "progn" "prog1" "prog2" "prog" "prog*"
    "multiple-value-bind" "multiple-value-call" "multiple-value-prog1"
    "setq" "setf" "psetq" "psetf" "incf" "decf" "push" "pop"
    "loop" "do" "do*" "dolist" "dotimes" "do-symbols" "do-external-symbols"
    "with-slots" "with-accessors" "with-open-file" "with-open-stream"
    "with-input-from-string" "with-output-to-string"
    "handler-case" "handler-bind" "restart-case" "restart-bind"
    "ignore-errors" "check-type" "assert"
    "declare" "the" "locally" "eval-when" "load-time-value"
    "in-package" "use-package" "import" "export"
    "and" "or" "not"
    "quote" "backquote")
  "List of special forms and common macros to highlight.")

(defparameter *builtins*
  '("car" "cdr" "cons" "list" "list*" "append" "reverse" "nreverse"
    "first" "second" "third" "fourth" "fifth" "rest" "nth" "nthcdr"
    "length" "elt" "aref" "svref" "gethash" "assoc" "member"
    "mapcar" "mapc" "maplist" "mapl" "mapcan" "mapcon"
    "remove" "delete" "remove-if" "delete-if" "remove-if-not" "delete-if-not"
    "find" "find-if" "find-if-not" "position" "position-if" "count" "count-if"
    "sort" "stable-sort" "merge"
    "reduce" "every" "some" "notevery" "notany"
    "format" "print" "prin1" "princ" "write" "read" "read-line"
    "funcall" "apply" "values" "values-list"
    "eq" "eql" "equal" "equalp" "null" "atom" "listp" "consp"
    "numberp" "integerp" "floatp" "stringp" "symbolp" "functionp"
    "type-of" "typep" "coerce"
    "make-instance" "slot-value" "class-of"
    "error" "warn" "signal" "cerror"
    "concatenate" "subseq" "string" "string-upcase" "string-downcase"
    "parse-integer" "read-from-string"
    "make-hash-table" "make-array" "make-list" "make-string"
    "get" "getf" "remf")
  "List of common built-in functions to highlight.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Character Classification
;;; ─────────────────────────────────────────────────────────────────────────────

(defun whitespace-char-p (char)
  "Return T if CHAR is whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun constituent-char-p (char)
  "Return T if CHAR is a Lisp constituent character (can be part of a symbol)."
  (and char
       (not (whitespace-char-p char))
       (not (member char '(#\( #\) #\' #\` #\, #\" #\; #\|)))))

(defun digit-char-10-p (char)
  "Return T if CHAR is a decimal digit."
  (and char (digit-char-p char 10)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token Types
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Token: (type start end)
;;; Types: :string :comment :keyword :number :special :builtin :paren :quote :symbol

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tokenizer
;;; ─────────────────────────────────────────────────────────────────────────────

(defun tokenize-lisp (string)
  "Tokenize STRING as Lisp code. Returns list of (type start end) tokens."
  (let ((tokens nil)
        (len (length string))
        (pos 0))
    (labels ((peek (&optional (offset 0))
               (let ((i (+ pos offset)))
                 (when (< i len)
                   (char string i))))
             (advance ()
               (when (< pos len)
                 (prog1 (char string pos)
                   (incf pos))))
             (push-token (type start)
               (push (list type start pos) tokens))
             (scan-string ()
               (let ((start pos))
                 (advance) ; skip opening "
                 (loop while (< pos len)
                       for char = (peek)
                       do (cond
                            ((char= char #\\)
                             (advance) ; skip backslash
                             (advance)) ; skip escaped char
                            ((char= char #\")
                             (advance) ; skip closing "
                             (return))
                            (t (advance))))
                 (push-token :string start)))
             (scan-line-comment ()
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (not (char= (peek) #\Newline)))
                       do (advance))
                 (push-token :comment start)))
             (scan-block-comment ()
               ;; #| ... |#
               (let ((start pos)
                     (depth 1))
                 (advance) ; skip #
                 (advance) ; skip |
                 (loop while (and (< pos len) (> depth 0))
                       do (cond
                            ((and (char= (peek) #\|)
                                  (< (1+ pos) len)
                                  (char= (peek 1) #\#))
                             (advance) (advance)
                             (decf depth))
                            ((and (char= (peek) #\#)
                                  (< (1+ pos) len)
                                  (char= (peek 1) #\|))
                             (advance) (advance)
                             (incf depth))
                            (t (advance))))
                 (push-token :comment start)))
             (scan-symbol ()
               (let ((start pos))
                 (loop while (and (< pos len)
                                  (constituent-char-p (peek)))
                       do (advance))
                 (let* ((text (subseq string start pos))
                        (lower (string-downcase text))
                        (type (cond
                                ((char= (char text 0) #\:) :keyword)
                                ((member lower *special-forms* :test #'string=) :special)
                                ((member lower *builtins* :test #'string=) :builtin)
                                (t :symbol))))
                   (push-token type start))))
             (scan-number ()
               (let ((start pos))
                 ;; Handle optional sign
                 (when (member (peek) '(#\+ #\-))
                   (advance))
                 ;; Scan digits and possible decimal/exponent
                 (loop while (and (< pos len)
                                  (or (digit-char-10-p (peek))
                                      (member (peek) '(#\. #\/ #\e #\E #\d #\D))))
                       do (advance))
                 ;; Check if we actually got a number
                 (if (> pos start)
                     (push-token :number start)
                     ;; Not a number, treat as symbol
                     (progn
                       (setf pos start)
                       (scan-symbol)))))
             (scan-hash ()
               ;; Handle # reader macros
               (let ((start pos))
                 (advance) ; skip #
                 (when (< pos len)
                   (let ((next (peek)))
                     (cond
                       ;; Block comment #| |#
                       ((char= next #\|)
                        (setf pos start)
                        (scan-block-comment))
                       ;; Character literal #\x
                       ((char= next #\\)
                        (advance) ; skip \
                        ;; Skip character name
                        (loop while (and (< pos len)
                                         (constituent-char-p (peek)))
                              do (advance))
                        (push-token :string start))
                       ;; Keyword #:foo
                       ((char= next #\:)
                        (advance) ; skip :
                        (loop while (and (< pos len)
                                         (constituent-char-p (peek)))
                              do (advance))
                        (push-token :keyword start))
                       ;; Other # things - just skip for now
                       (t
                        (loop while (and (< pos len)
                                         (constituent-char-p (peek)))
                              do (advance))
                        (push-token :symbol start))))))))
      ;; Main tokenization loop
      (loop while (< pos len)
            for char = (peek)
            do (cond
                 ;; Whitespace - skip
                 ((whitespace-char-p char)
                  (advance))
                 ;; String
                 ((char= char #\")
                  (scan-string))
                 ;; Line comment
                 ((char= char #\;)
                  (scan-line-comment))
                 ;; Parentheses
                 ((member char '(#\( #\)))
                  (let ((start pos))
                    (advance)
                    (push-token :paren start)))
                 ;; Quote characters
                 ((member char '(#\' #\` #\,))
                  (let ((start pos))
                    (advance)
                    ;; Handle ,@
                    (when (and (char= char #\,)
                               (< pos len)
                               (char= (peek) #\@))
                      (advance))
                    (push-token :quote start)))
                 ;; Hash reader macros
                 ((char= char #\#)
                  (scan-hash))
                 ;; Keyword (starting with colon)
                 ((char= char #\:)
                  (scan-symbol))
                 ;; Number (starting with digit or sign followed by digit)
                 ((or (digit-char-10-p char)
                      (and (member char '(#\+ #\-))
                           (< (1+ pos) len)
                           (digit-char-10-p (peek 1))))
                  (scan-number))
                 ;; Symbol
                 ((constituent-char-p char)
                  (scan-symbol))
                 ;; Unknown - skip
                 (t (advance)))))
    (nreverse tokens)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Paren Matching
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-matching-paren (string pos tokens)
  "Find the position of the paren matching the one at POS in STRING.
   TOKENS is the list of tokens from tokenize-lisp.
   Returns the position of the matching paren, or NIL if not found/unmatched."
  (when (and (>= pos 0) (< pos (length string)))
    (let ((char (char string pos)))
      (cond
        ;; Opening paren - search forward
        ((char= char #\()
         (let ((depth 1)
               (i (1+ pos)))
           (loop while (and (< i (length string)) (> depth 0))
                 for c = (char string i)
                 do (cond
                      ;; Skip if inside string or comment
                      ((position-in-string-or-comment-p i tokens)
                       (incf i))
                      ((char= c #\()
                       (incf depth)
                       (incf i))
                      ((char= c #\))
                       (decf depth)
                       (if (zerop depth)
                           (return-from find-matching-paren i)
                           (incf i)))
                      (t (incf i))))
           nil))
        ;; Closing paren - search backward
        ((char= char #\))
         (let ((depth 1)
               (i (1- pos)))
           (loop while (and (>= i 0) (> depth 0))
                 for c = (char string i)
                 do (cond
                      ;; Skip if inside string or comment
                      ((position-in-string-or-comment-p i tokens)
                       (decf i))
                      ((char= c #\))
                       (incf depth)
                       (decf i))
                      ((char= c #\()
                       (decf depth)
                       (if (zerop depth)
                           (return-from find-matching-paren i)
                           (decf i)))
                      (t (decf i))))
           nil))
        (t nil)))))

(defun position-in-string-or-comment-p (pos tokens)
  "Return T if POS is inside a string or comment token."
  (dolist (token tokens)
    (destructuring-bind (type start end) token
      (when (and (member type '(:string :comment))
                 (> pos start)
                 (< pos end))
        (return-from position-in-string-or-comment-p t))))
  nil)

(defun find-paren-at-or-before-cursor (string cursor-pos)
  "Find a paren at cursor or immediately before it.
   Returns the position of the paren to match, or NIL."
  (when cursor-pos
    (cond
      ;; Check at cursor position
      ((and (< cursor-pos (length string))
            (member (char string cursor-pos) '(#\( #\))))
       cursor-pos)
      ;; Check before cursor (for when cursor is after closing paren)
      ((and (> cursor-pos 0)
            (member (char string (1- cursor-pos)) '(#\( #\))))
       (1- cursor-pos))
      (t nil))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Highlighter
;;; ─────────────────────────────────────────────────────────────────────────────

(defun token-color (type)
  "Return ANSI color code for token TYPE."
  (case type
    (:string *hl-string*)
    (:comment *hl-comment*)
    (:keyword *hl-keyword*)
    (:number *hl-number*)
    (:special *hl-special*)
    (:builtin *hl-builtin*)
    (:paren *hl-paren*)
    (:quote *hl-quote*)
    (otherwise nil)))

(defun highlight-string (string &optional cursor-pos)
  "Apply syntax highlighting to STRING. Returns highlighted string.
   If CURSOR-POS is provided, also highlights matching parentheses."
  (if (or (not *syntax-highlighting-enabled*)
          (not (colors-enabled-p))
          (zerop (length string)))
      string
      (let* ((tokens (tokenize-lisp string))
             ;; Find paren to match at/near cursor
             (paren-pos (when cursor-pos
                          (find-paren-at-or-before-cursor string cursor-pos)))
             ;; Find its match
             (match-pos (when paren-pos
                          (find-matching-paren string paren-pos tokens)))
             ;; Positions to highlight (could be nil, one, or both)
             (highlight-positions (remove nil (list paren-pos match-pos))))
        (with-output-to-string (out)
          (let ((last-end 0))
            (dolist (token tokens)
              (destructuring-bind (type start end) token
                ;; Output unhighlighted text before this token
                (when (> start last-end)
                  (write-string (subseq string last-end start) out))
                ;; Check if this is a paren token that needs match highlighting
                (let ((color (if (and (eq type :paren)
                                      (member start highlight-positions))
                                 (if match-pos
                                     *hl-paren-match*      ; Both parens found - highlight match
                                     *hl-paren-mismatch*)  ; No match - highlight as error
                                 (token-color type))))
                  (if color
                      (format out "~A~A~A"
                              color
                              (subseq string start end)
                              *hl-reset*)
                      (write-string (subseq string start end) out)))
                (setf last-end end)))
            ;; Output any remaining text
            (when (< last-end (length string))
              (write-string (subseq string last-end) out)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Display Width Calculation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun display-width (string)
  "Calculate the display width of STRING, ignoring ANSI escape sequences."
  (let ((width 0)
        (in-escape nil)
        (len (length string)))
    (dotimes (i len)
      (let ((char (char string i)))
        (cond
          ;; Start of escape sequence
          ((and (not in-escape) (char= char #\Escape))
           (setf in-escape t))
          ;; End of escape sequence (letter terminates CSI sequences)
          ((and in-escape (alpha-char-p char))
           (setf in-escape nil))
          ;; Normal character (not in escape)
          ((not in-escape)
           (incf width)))))
    width))
