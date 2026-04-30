;;; paredit.lisp --- Paredit-style structural editing for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *paredit-mode* nil
  "Enable/disable paredit-style structural editing.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Delimiter Pairs
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *open-delimiters* '(#\( #\[ #\{)
  "Opening delimiter characters.")

(defparameter *close-delimiters* '(#\) #\] #\})
  "Closing delimiter characters.")

(defparameter *delimiter-pairs*
  '((#\( . #\))
    (#\[ . #\])
    (#\{ . #\}))
  "Mapping from opening to closing delimiters.")

(defun open-delimiter-p (char)
  "Return T if CHAR is an opening delimiter."
  (member char *open-delimiters*))

(defun close-delimiter-p (char)
  "Return T if CHAR is a closing delimiter."
  (member char *close-delimiters*))

(defun matching-close (open-char)
  "Return the closing delimiter that matches OPEN-CHAR."
  (cdr (assoc open-char *delimiter-pairs*)))

(defun matching-open (close-char)
  "Return the opening delimiter that matches CLOSE-CHAR."
  (car (rassoc close-char *delimiter-pairs*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String/Comment Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defun in-string-p (string pos)
  "Return T if position POS in STRING is inside a string literal."
  (let ((in-string nil)
        (i 0)
        (limit (min pos (length string))))
    (loop while (< i limit)
          for char = (char string i)
          do (cond
               ((and (char= char #\\) in-string)
                (incf i)) ; skip escaped char in string
               ((char= char #\")
                (setf in-string (not in-string))))
             (incf i))
    in-string))

(defun in-comment-p (string pos)
  "Return T if position POS in STRING is in a comment."
  (let ((line-start (or (position #\Newline string :end pos :from-end t) -1)))
    (let ((semicolon-pos (position #\; string :start (1+ line-start) :end pos)))
      (and semicolon-pos
           (not (in-string-p string semicolon-pos))))))

(defun in-string-or-comment-p (string pos)
  "Return T if position POS is inside a string or comment."
  (or (in-string-p string pos)
      (in-comment-p string pos)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Balance Checking
;;; ─────────────────────────────────────────────────────────────────────────────

(defun count-unmatched-delimiters (string)
  "Count unmatched opening delimiters in STRING.
   Returns (values open-count close-count) for parens only."
  (let ((depth 0)
        (in-string nil)
        (i 0)
        (len (length string)))
    (loop while (< i len)
          for char = (char string i)
          do (cond
               ;; Handle escape in string
               ((and in-string (char= char #\\) (< (1+ i) len))
                (incf i))
               ;; Toggle string state
               ((char= char #\")
                (setf in-string (not in-string)))
               ;; Skip if in string
               (in-string nil)
               ;; Count parens
               ((char= char #\()
                (incf depth))
               ((char= char #\))
                (decf depth)))
             (incf i))
    depth))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Balanced Insertion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun paredit-open-delimiter (buf char)
  "Insert opening delimiter CHAR and its matching close.
   Cursor ends up between them."
  (let ((close (matching-close char)))
    (buffer-insert-char buf char)
    (buffer-insert-char buf close)
    (buffer-move-left buf)))

(defun paredit-close-delimiter (buf char)
  "Handle closing delimiter CHAR.
   If cursor is on matching close, just move past it.
   Otherwise insert it normally."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf)))
    (if (and (< pos (length content))
             (char= (char content pos) char))
        ;; On closing delimiter - just move past it
        (buffer-move-right buf)
        ;; Not on closing delimiter - insert normally
        (buffer-insert-char buf char))))

(defun paredit-double-quote (buf)
  "Handle double quote insertion.
   If in string and on closing quote, move past it.
   Otherwise insert pair of quotes."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf)))
    (cond
      ;; On a closing quote while in string - move past
      ((and (< pos (length content))
            (char= (char content pos) #\")
            (in-string-p content pos))
       (buffer-move-right buf))
      ;; In string - just insert single quote
      ((in-string-p content pos)
       (buffer-insert-char buf #\"))
      ;; Not in string - insert pair
      (t
       (buffer-insert-char buf #\")
       (buffer-insert-char buf #\")
       (buffer-move-left buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Safe Deletion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun paredit-backspace (buf)
  "Handle backspace with paredit rules.
   - Delete empty pairs () [] {} \"\"
   - Don't delete opening delimiter if it would unbalance
   Returns T if something was deleted, NIL if blocked."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf)))
    (when (zerop pos)
      (return-from paredit-backspace nil))
    (let ((char-before (char content (1- pos))))
      (cond
        ;; Empty pair - delete both
        ((and (< pos (length content))
              (or (and (char= char-before #\()
                       (char= (char content pos) #\)))
                  (and (char= char-before #\[)
                       (char= (char content pos) #\]))
                  (and (char= char-before #\{)
                       (char= (char content pos) #\}))
                  (and (char= char-before #\")
                       (char= (char content pos) #\"))))
         ;; Delete both characters
         (buffer-delete-char-before buf)
         (buffer-delete-char-at buf)
         t)
        ;; Opening delimiter with content - block deletion
        ((and (open-delimiter-p char-before)
              (not (in-string-p content (1- pos))))
         nil)
        ;; Closing delimiter - block deletion
        ((and (close-delimiter-p char-before)
              (not (in-string-p content (1- pos))))
         nil)
        ;; Normal deletion
        (t
         (buffer-delete-char-before buf))))))

(defun paredit-delete (buf)
  "Handle delete key with paredit rules.
   - Delete empty pairs
   - Don't delete closing delimiter if it would unbalance
   Returns T if something was deleted, NIL if blocked."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf)))
    (when (>= pos (length content))
      (return-from paredit-delete nil))
    (let ((char-at (char content pos)))
      (cond
        ;; Empty pair - delete both
        ((and (plusp pos)
              (or (and (char= (char content (1- pos)) #\()
                       (char= char-at #\)))
                  (and (char= (char content (1- pos)) #\[)
                       (char= char-at #\]))
                  (and (char= (char content (1- pos)) #\{)
                       (char= char-at #\}))
                  (and (char= (char content (1- pos)) #\")
                       (char= char-at #\"))))
         (buffer-delete-char-before buf)
         (buffer-delete-char-at buf)
         t)
        ;; Opening delimiter - block deletion
        ((and (open-delimiter-p char-at)
              (not (in-string-p content pos)))
         nil)
        ;; Closing delimiter - block deletion
        ((and (close-delimiter-p char-at)
              (not (in-string-p content pos)))
         nil)
        ;; Normal deletion
        (t
         (buffer-delete-char-at buf))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; S-expression Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun skip-whitespace-forward (string pos)
  "Skip whitespace forward from POS in STRING. Returns new position."
  (loop while (and (< pos (length string))
                   (member (char string pos) '(#\Space #\Tab #\Newline)))
        do (incf pos))
  pos)

(defun skip-whitespace-backward (string pos)
  "Skip whitespace backward from POS in STRING. Returns new position."
  (loop while (and (plusp pos)
                   (member (char string (1- pos)) '(#\Space #\Tab #\Newline)))
        do (decf pos))
  pos)

(defun find-sexp-end (string start)
  "Find the end position of the S-expression starting at START.
   Returns the position after the sexp, or NIL if invalid."
  (when (>= start (length string))
    (return-from find-sexp-end nil))
  (let ((pos start)
        (char (char string start)))
    (cond
      ;; Opening delimiter - find matching close
      ((open-delimiter-p char)
       (let ((depth 1)
             (open char)
             (close (matching-close char)))
         (incf pos)
         (loop while (and (< pos (length string)) (plusp depth))
               for c = (char string pos)
               do (cond
                    ((and (char= c #\\) (< (1+ pos) (length string)))
                     (incf pos 2))
                    ((char= c #\")
                     ;; Skip string
                     (incf pos)
                     (loop while (and (< pos (length string))
                                      (not (char= (char string pos) #\")))
                           do (when (char= (char string pos) #\\)
                                (incf pos))
                              (incf pos))
                     (when (< pos (length string))
                       (incf pos)))
                    ((char= c open)
                     (incf depth)
                     (incf pos))
                    ((char= c close)
                     (decf depth)
                     (incf pos))
                    (t (incf pos))))
         (if (zerop depth) pos nil)))
      ;; String
      ((char= char #\")
       (incf pos)
       (loop while (and (< pos (length string))
                        (not (char= (char string pos) #\")))
             do (when (char= (char string pos) #\\)
                  (incf pos))
                (incf pos))
       (if (< pos (length string)) (1+ pos) nil))
      ;; Quote/backquote - include following sexp
      ((member char '(#\' #\` #\,))
       (incf pos)
       (when (and (< pos (length string)) (char= (char string pos) #\@))
         (incf pos))
       (setf pos (skip-whitespace-forward string pos))
       (find-sexp-end string pos))
      ;; Symbol/number - read until delimiter or whitespace
      (t
       (loop while (and (< pos (length string))
                        (not (member (char string pos)
                                     '(#\Space #\Tab #\Newline
                                       #\( #\) #\[ #\] #\{ #\}
                                       #\' #\" #\` #\, #\;))))
             do (incf pos))
       pos))))

(defun find-sexp-start (string end)
  "Find the start position of the S-expression ending at END.
   Returns the position at the start of the sexp, or NIL if invalid."
  (when (zerop end)
    (return-from find-sexp-start nil))
  (let ((pos (1- end)))
    ;; Skip whitespace backward
    (setf pos (skip-whitespace-backward string (1+ pos)))
    (when (zerop pos)
      (return-from find-sexp-start 0))
    (decf pos)
    (let ((char (char string pos)))
      (cond
        ;; Closing delimiter - find matching open
        ((close-delimiter-p char)
         (let ((depth 1)
               (close char)
               (open (matching-open char)))
           (decf pos)
           (loop while (and (>= pos 0) (plusp depth))
                 for c = (char string pos)
                 do (cond
                      ((char= c close)
                       (incf depth)
                       (decf pos))
                      ((char= c open)
                       (decf depth)
                       (unless (zerop depth)
                         (decf pos)))
                      ((char= c #\")
                       ;; Skip string backward
                       (decf pos)
                       (loop while (and (>= pos 0)
                                        (not (and (char= (char string pos) #\")
                                                  (or (zerop pos)
                                                      (not (char= (char string (1- pos)) #\\))))))
                             do (decf pos))
                       (when (>= pos 0) (decf pos)))
                      (t (decf pos))))
           (if (zerop depth) pos nil)))
        ;; String - find opening quote
        ((char= char #\")
         (decf pos)
         (loop while (and (>= pos 0)
                          (not (and (char= (char string pos) #\")
                                    (or (zerop pos)
                                        (not (char= (char string (1- pos)) #\\))))))
               do (decf pos))
         (if (>= pos 0) pos nil))
        ;; Symbol/number - find start
        (t
         (loop while (and (>= pos 0)
                          (not (member (char string pos)
                                       '(#\Space #\Tab #\Newline
                                         #\( #\) #\[ #\] #\{ #\}
                                         #\' #\" #\` #\, #\;))))
               do (decf pos))
         (1+ pos))))))

(defun buffer-forward-sexp (buf)
  "Move cursor forward over one S-expression. Returns T if moved."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf))
         (pos (skip-whitespace-forward content pos)))
    (when (>= pos (length content))
      (return-from buffer-forward-sexp nil))
    (let ((end (find-sexp-end content pos)))
      (when end
        (buffer-set-cursor-position buf end)
        t))))

(defun buffer-backward-sexp (buf)
  "Move cursor backward over one S-expression. Returns T if moved."
  (let* ((content (buffer-contents buf))
         (pos (buffer-cursor-position buf))
         (pos (skip-whitespace-backward content pos)))
    (when (zerop pos)
      (return-from buffer-backward-sexp nil))
    (let ((start (find-sexp-start content pos)))
      (when start
        (buffer-set-cursor-position buf start)
        t))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Buffer Position Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun buffer-set-cursor-position (buf absolute-pos)
  "Set cursor to ABSOLUTE-POS in buffer content."
  (let ((pos 0)
        (row 0))
    (loop while (< row (buffer-line-count buf))
          for line-len = (length (buffer-line buf row))
          do (if (<= absolute-pos (+ pos line-len))
                 (progn
                   (setf (edit-buffer-row buf) row)
                   (setf (edit-buffer-col buf) (- absolute-pos pos))
                   (return-from buffer-set-cursor-position t))
                 (progn
                   (incf pos (1+ line-len)) ; +1 for newline
                   (incf row))))
    ;; Position at end if past content
    (buffer-move-to-end buf)
    t))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main Entry Point
;;; ─────────────────────────────────────────────────────────────────────────────

(defun paredit-handle-char (buf char)
  "Handle character input with paredit rules.
   Returns :handled if paredit handled it, NIL for normal processing."
  (unless *paredit-mode*
    (return-from paredit-handle-char nil))
  (cond
    ;; Opening delimiters - insert pair
    ((open-delimiter-p char)
     (paredit-open-delimiter buf char)
     :handled)
    ;; Closing delimiters - skip if on one
    ((close-delimiter-p char)
     (paredit-close-delimiter buf char)
     :handled)
    ;; Double quote
    ((char= char #\")
     (paredit-double-quote buf)
     :handled)
    ;; Normal character
    (t nil)))
