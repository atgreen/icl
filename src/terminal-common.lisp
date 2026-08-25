;;; terminal-common.lisp --- Platform-independent terminal helpers
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; Shared by terminal-posix.lisp and terminal-windows.lisp, which are
;;; loaded mutually exclusively.  This file is loaded after whichever
;;; platform file applies, so it can rely on READ-CHAR-RAW and +ESC+.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; SGR Mouse Tracking
;;; ─────────────────────────────────────────────────────────────────────────────

(defun parse-sgr-mouse ()
  "Parse an SGR mouse event after ESC [ < has been read.
   Returns (:mouse action button x y) or :unknown.
   ACTION is :press, :release, or :drag. BUTTON is :left, :middle, :right,
   :wheel-up, or :wheel-down. X and Y are 1-based terminal coordinates."
  (let ((nums (list 0))
        (c nil))
    (loop do (setf c (read-char-raw))
          while c
          do (cond
               ((digit-char-p c)
                (setf (car nums)
                      (+ (* (car nums) 10)
                         (- (char-code c) (char-code #\0)))))
               ((char= c #\;)
                (push 0 nums))
               (t (return))))
    (unless (and c
                 (or (char= c #\M) (char= c #\m))
                 (>= (length nums) 3))
      (return-from parse-sgr-mouse :unknown))
    (let* ((nums (nreverse nums))
           (btn (first nums))
           (x (second nums))
           (y (third nums))
           (release (char= c #\m))
           (motion (logtest 32 btn))
           (button (cond
                     ((logtest 64 btn)
                      (if (logtest 1 btn) :wheel-down :wheel-up))
                     ((= (logand btn 3) 0) :left)
                     ((= (logand btn 3) 1) :middle)
                     ((= (logand btn 3) 2) :right)
                     (t :left)))
           (action (cond
                     (release :release)
                     (motion :drag)
                     (t :press))))
      (list :mouse action button x y))))

(defun enable-mouse-tracking ()
  "Enable SGR mouse tracking (clicks and drags)."
  (unless (or *browser-terminal-active* *mouse-tracking-enabled*)
    (format t "~C[?1000h~C[?1002h~C[?1006h" +esc+ +esc+ +esc+)
    (force-output)
    (setf *mouse-tracking-enabled* t)))

(defun disable-mouse-tracking ()
  "Disable SGR mouse tracking."
  (when *mouse-tracking-enabled*
    (format t "~C[?1006l~C[?1002l~C[?1000l" +esc+ +esc+ +esc+)
    (force-output)
    (setf *mouse-tracking-enabled* nil)))
