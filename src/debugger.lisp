;;; debugger.lisp --- Interactive debugger TUI for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debugger State
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct debugger-state
  "State for the interactive debugger."
  (condition-type "ERROR" :type string)
  (condition-message "" :type string)
  (restarts nil :type list)           ; ((name desc) ...)
  (frames nil :type list)             ; list of frame description strings
  (selected 0 :type fixnum)
  (scroll-offset 0 :type fixnum)      ; scroll position for current view
  (max-visible 15 :type fixnum)       ; max items visible in the box
  (view :backtrace :type keyword)     ; :backtrace, :restarts, or :locals
  (live-p nil :type boolean)          ; T when invoked from live debug session
  (thread nil)                        ; backend thread ID (live mode)
  (level nil)                         ; debug level (live mode)
  (raw-frames nil :type list)         ; original frame tuples from slynk
  (locals nil :type list)             ; current frame's locals: ((:name N :id I :value V) ...)
  (locals-frame-index nil))           ; which frame's locals are shown

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Parse backtrace into frame list
;;; ─────────────────────────────────────────────────────────────────────────────

(defun normalize-backtrace-frames (backtrace)
  "Convert backtrace data into a list of display strings.
   Handles both slynk:backtrace format ((index string plist) ...) and plain strings."
  (cond
    ((null backtrace) nil)
    ;; Plain string: split into lines
    ((stringp backtrace)
     (let ((lines (split-sequence:split-sequence #\Newline backtrace)))
       (remove-if (lambda (line) (string= (string-trim '(#\Space #\Tab) line) ""))
                  lines)))
    ;; List of frame tuples from slynk:backtrace: ((idx "desc" plist) ...)
    ((and (listp backtrace) (listp (first backtrace)))
     (mapcar (lambda (frame)
               (format nil "~D: ~A" (first frame) (second frame)))
             backtrace))
    ;; Unknown format: convert each element to string
    ((listp backtrace)
     (mapcar #'princ-to-string backtrace))
    (t nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Frame Locals
;;; ─────────────────────────────────────────────────────────────────────────────

(defun debugger-fetch-frame-locals (frame-index thread)
  "Fetch locals for FRAME-INDEX from the backend using THREAD.
   Returns the locals list or nil."
  (handler-case
      (let ((result (debugger-eval-in-thread
                     `(slynk:frame-locals-and-catch-tags ,frame-index)
                     thread)))
        ;; Result is (LOCALS TAGS), return locals
        (when (and (listp result) (first result))
          (first result)))
    (error () nil)))

(defun debugger-get-real-frame-index (state)
  "Get the real slynk frame index for the currently selected backtrace frame."
  (let ((raw-frames (debugger-state-raw-frames state))
        (selected (debugger-state-selected state)))
    (when (and raw-frames (< selected (length raw-frames)))
      (first (nth selected raw-frames)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Interactive Restarts
;;; ─────────────────────────────────────────────────────────────────────────────

(defun debugger-restart-needs-value-p (restart-name)
  "Return T if the restart named RESTART-NAME requires a user-provided value."
  (and (stringp restart-name)
       (let ((upper (string-upcase restart-name)))
         (or (string= upper "USE-VALUE")
             (string= upper "STORE-VALUE")))))

(defun debugger-prompt-restart-value ()
  "Prompt the user for a value for an interactive restart.
   Returns the value string, or NIL if cancelled.
   Must be called from within WITH-RAW-MODE."
  (exit-raw-mode)
  (clear-debugger)
  (let ((lines-to-clear 1))
    (unwind-protect
         (progn
           (format t "~&Value (Lisp expression): ")
           (force-output)
           (let ((input (read-line *standard-input* nil nil)))
             (if (and input (plusp (length (string-trim " " input))))
                 input
                 nil)))
      (when (> lines-to-clear 0)
        (format t "~C[~DA~C[G~C[J" #\Escape lines-to-clear #\Escape #\Escape)
        (force-output))
      (enter-raw-mode)
      (setf *debugger-lines-drawn* 0))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Eval in Frame
;;; ─────────────────────────────────────────────────────────────────────────────

(defun debugger-eval-in-frame (state)
  "Evaluate a form in the context of the current frame.
   Works in both :backtrace and :locals views."
  (let* ((thread (debugger-state-thread state))
         (frame-index (if (eq (debugger-state-view state) :locals)
                          (debugger-state-locals-frame-index state)
                          (debugger-get-real-frame-index state))))
    (unless (and thread frame-index)
      (return-from debugger-eval-in-frame))
    ;; Exit raw mode for line input
    (exit-raw-mode)
    (clear-debugger)
    (let ((lines-to-clear 1))  ; at minimum the prompt line
      (unwind-protect
           (progn
             (format t "~&Eval in frame ~D (* = frame): " frame-index)
             (force-output)
             (let ((form-string (read-line *standard-input* nil nil)))
               (when (and form-string (plusp (length (string-trim " " form-string))))
                 (let ((result-string
                         (handler-case
                             (princ-to-string
                              (debugger-eval-in-thread
                               `(slynk:eval-string-in-frame
                                 ,form-string ,frame-index "CL-USER")
                               thread))
                           (error (e)
                             (format nil "Error: ~A" e)))))
                   (format t "~&~%=> ~A~%~%" result-string)
                   (format t "~&Press any key to continue...")
                   (force-output)
                   ;; Track lines: blank + result lines + blank + press-key line
                   (incf lines-to-clear (+ 3 (count #\Newline result-string)))
                   (enter-raw-mode)
                   (read-key)
                   (exit-raw-mode)))))
        ;; Clear eval output before returning to debugger
        (when (> lines-to-clear 0)
          (format t "~C[~DA~C[G~C[J" #\Escape lines-to-clear #\Escape #\Escape)
          (force-output))
        (enter-raw-mode)
        (setf *debugger-lines-drawn* 0)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspect Frame Variable
;;; ─────────────────────────────────────────────────────────────────────────────

(defun debugger-inspect-frame-var (state)
  "Inspect the currently selected local variable via the inspector."
  (let* ((thread (debugger-state-thread state))
         (frame-index (debugger-state-locals-frame-index state))
         (locals (debugger-state-locals state))
         (selected (debugger-state-selected state)))
    (unless (and thread frame-index locals (< selected (length locals)))
      (return-from debugger-inspect-frame-var))
    (let* ((local (nth selected locals))
           (var-id (getf local :id)))
      (unless var-id
        (return-from debugger-inspect-frame-var))
      ;; Initialize inspector state on the backend via the debug thread
      (let ((data (handler-case
                      (debugger-eval-in-thread
                       `(slynk:inspect-frame-var ,frame-index ,var-id)
                       thread)
                    (error () nil))))
        (when data
          ;; Clear debugger display before showing inspector
          (clear-debugger)
          ;; Set up and run inspector
          (setf *inspector* (make-inspector-state))
          (setf *inspector-lines-drawn* 0)
          (update-inspector-state data)
          (let ((first-render t))
            (loop
              (render-inspector first-render)
              (setf first-render nil)
              (let ((key (read-key)))
                (cond
                  ((or (eql key #\q) (eql key #\Q) (eql key :eof))
                   (return))
                  ((or (eql key #\b) (eql key #\B) (eql key :backspace) (eql key :escape))
                   (if (inspector-state-path *inspector*)
                       (inspector-go-back)
                       (return)))
                  ((eql key :enter)
                   (inspector-drill-down))
                  ((eql key :up)
                   (inspector-move :up))
                  ((eql key :down)
                   (inspector-move :down))
                  ((or (eql key #\k) (eql key #\K))
                   (inspector-move :up))
                  ((or (eql key #\j) (eql key #\J))
                   (inspector-move :down))
                  ((eql key #\h)
                   (inspector-nav-sibling :left))
                  ((eql key #\l)
                   (inspector-nav-sibling :right))
                  ((eql key #\a)
                   (inspector-nav-car))
                  ((eql key #\d)
                   (inspector-nav-cdr))
                  ((eql key #\u)
                   (inspector-nav-up))
                  ((eql key #\[)
                   (inspector-history-back))
                  ((eql key #\])
                   (inspector-history-forward))
                  ((eql key #\e)
                   (inspector-eval-in-context))))))
          ;; Clear inspector on exit
          (clear-inspector)
          ;; Force full debugger redraw
          (setf *debugger-lines-drawn* 0))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Rendering
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *debugger-lines-drawn* 0
  "Number of lines drawn in the last debugger render.")

(defun debugger-current-items (state)
  "Return the list of items for the current view."
  (case (debugger-state-view state)
    (:backtrace (debugger-state-frames state))
    (:restarts (debugger-state-restarts state))
    (:locals (debugger-state-locals state))
    (otherwise nil)))

(defun render-debugger (&optional first-render)
  "Render the debugger state to the terminal."
  (let* ((state *debugger-state*)
         (items (debugger-current-items state))
         (selected (debugger-state-selected state))
         (view-mode (debugger-state-view state))
         (lines-to-draw 0))
    ;; Move cursor back to overwrite previous render
    (unless first-render
      (when (> *debugger-lines-drawn* 0)
        (format t "~C[~DA" #\Escape *debugger-lines-drawn*)))
    ;; Header: condition in a border
    (let* ((header-text (format nil "~A~%~A"
                                (tui:colored (tui:bold (debugger-state-condition-type state))
                                             :fg (if *color-error*
                                                     (tuition:resolve-color *color-error*)
                                                     tui:*fg-bright-red*))
                                (tui:colored (debugger-state-condition-message state)
                                             :fg (if *color-warning*
                                                     (tuition:resolve-color *color-warning*)
                                                     tui:*fg-yellow*))))
           (header (tui:render-border header-text tui:*border-rounded*
                                      :title "Debugger"
                                      :title-position :center))
           (header-lines (tui:split-string-by-newline header)))
      (dolist (line header-lines)
        (format t "~C[2K~A~%" #\Escape line)
        (incf lines-to-draw)))
    ;; Tab indicator (use bold/dim for theme safety)
    (let* ((bt-label (if (eq view-mode :backtrace)
                         (format nil "~A[Backtrace]~A" *ansi-bold* *ansi-reset*)
                         (format nil "~ABacktrace~A" *ansi-dim* *ansi-reset*)))
           (rst-count (length (debugger-state-restarts state)))
           (rst-label (if (eq view-mode :restarts)
                          (format nil "~A[Restarts (~D)]~A" *ansi-bold* rst-count *ansi-reset*)
                          (format nil "~ARestarts (~D)~A" *ansi-dim* rst-count *ansi-reset*)))
           (locals-label (when (eq view-mode :locals)
                           (format nil "~A[Frame ~A Locals]~A"
                                   *ansi-bold*
                                   (or (debugger-state-locals-frame-index state) "?")
                                   *ansi-reset*))))
      (if locals-label
          (format t "~C[2K ~A  ~A  ~A~%" #\Escape bt-label rst-label locals-label)
          (format t "~C[2K ~A  ~A~%" #\Escape bt-label rst-label))
      (incf lines-to-draw))
    ;; Bordered item list with scroll windowing
    (let* ((total (length items))
           (scroll (debugger-state-scroll-offset state))
           (max-vis (debugger-state-max-visible state))
           (width (multiple-value-bind (cols rows)
                      (get-terminal-size)
                    (declare (ignore rows))
                    (or cols 80)))
           (visible-count (min max-vis (max 0 (- total scroll)))))
      ;; Top border with title
      (format t "~C[2K" #\Escape)
      (let* ((title-text (case view-mode
                           (:backtrace
                            (if (plusp total)
                                (format nil "Backtrace [~D/~D]" (1+ selected) total)
                                "Backtrace"))
                           (:restarts
                            (format nil "Restarts (~D)" total))
                           (:locals
                            (let ((count (length (debugger-state-locals state))))
                              (format nil "Frame ~A Locals (~D)"
                                      (or (debugger-state-locals-frame-index state) "?")
                                      count)))
                           (otherwise "")))
             (display-title (truncate-string title-text (- width 6))))
        (format t "~A┌─ ~A ~A┐~A~%"
                *ansi-bold*
                display-title
                (make-string (max 0 (- width 5 (length display-title)))
                             :initial-element #\─)
                *ansi-reset*))
      (incf lines-to-draw)
      ;; Visible items
      (if (plusp total)
          (dotimes (i visible-count)
            (format t "~C[2K" #\Escape)
            (let* ((idx (+ scroll i))
                   (item (nth idx items))
                   (selected-p (= idx selected))
                   (prefix (if selected-p " > " "   "))
                   (content (case view-mode
                              (:restarts
                               (format nil "~A: ~A" (first item) (second item)))
                              (:locals
                               (format nil "~A = ~A"
                                       (getf item :name)
                                       (getf item :value)))
                              (otherwise
                               (princ-to-string item))))
                   (truncated (truncate-string content (- width 8))))
              (if selected-p
                  (format t "│~A~A~A~VA~A│~%"
                          *ansi-reverse* prefix truncated
                          (max 0 (- width 5 (length truncated))) ""
                          *ansi-reset*)
                  (format t "│~A~A~VA│~%"
                          prefix truncated
                          (max 0 (- width 5 (length truncated))) "")))
            (incf lines-to-draw))
          (progn
            (format t "~C[2K│~A   (none)~A~VA│~%"
                    #\Escape *ansi-dim* *ansi-reset*
                    (max 0 (- width 11)) "")
            (incf lines-to-draw)))
      ;; Pad remaining lines
      (dotimes (i (- max-vis (if (plusp total) visible-count 1)))
        (format t "~C[2K│~VA│~%" #\Escape (- width 2) "")
        (incf lines-to-draw))
      ;; Bottom border
      (format t "~C[2K└~A┘~%" #\Escape
              (make-string (- width 2) :initial-element #\─))
      (incf lines-to-draw))
    ;; Footer
    (format t "~C[2K~A~A~A~%" #\Escape
            *ansi-dim* (debugger-footer-text state) *ansi-reset*)
    (incf lines-to-draw)
    ;; Clear leftover lines from previous render (e.g. switching from backtrace to restarts)
    (when (> *debugger-lines-drawn* lines-to-draw)
      (loop repeat (- *debugger-lines-drawn* lines-to-draw)
            do (format t "~C[2K~%" #\Escape)
               (incf lines-to-draw)))
    (force-output)
    (setf *debugger-lines-drawn* lines-to-draw)))

(defun debugger-footer-text (state)
  "Return the footer help text for the current debugger state."
  (let ((live-p (debugger-state-live-p state))
        (view (debugger-state-view state)))
    (cond
      ((not live-p)
       " ↑/↓ navigate  Tab switch view  q quit")
      ((eq view :locals)
       " ↑/↓ navigate  Enter inspect  e eval  Esc back  q abort")
      (t
       " ↑/↓ navigate  Tab switch  Enter locals/restart  e eval  q abort"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Display Clearing
;;; ─────────────────────────────────────────────────────────────────────────────

(defun clear-debugger ()
  "Clear the debugger display."
  (when (> *debugger-lines-drawn* 0)
    (format t "~C[~DA~C[G~C[2K~C[J"
            #\Escape *debugger-lines-drawn* #\Escape #\Escape #\Escape)
    (setf *debugger-lines-drawn* 0)
    (force-output)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun debugger-move (direction)
  "Move selection in DIRECTION (:up or :down), adjusting scroll offset."
  (let* ((state *debugger-state*)
         (items (debugger-current-items state))
         (total (length items))
         (selected (debugger-state-selected state))
         (max-visible (debugger-state-max-visible state))
         (scroll (debugger-state-scroll-offset state)))
    (when (zerop total) (return-from debugger-move))
    (let ((new-selected (case direction
                          (:up (max 0 (1- selected)))
                          (:down (min (1- total) (1+ selected)))
                          (otherwise selected))))
      (setf (debugger-state-selected state) new-selected)
      ;; Adjust scroll
      (when (< new-selected scroll)
        (setf (debugger-state-scroll-offset state) new-selected))
      (when (>= new-selected (+ scroll max-visible))
        (setf (debugger-state-scroll-offset state)
              (- new-selected max-visible -1))))))

(defun debugger-switch-to-locals (state)
  "Fetch locals for the selected backtrace frame and switch to :locals view."
  (let* ((thread (debugger-state-thread state))
         (frame-index (debugger-get-real-frame-index state)))
    (when (and thread frame-index)
      (let ((locals (debugger-fetch-frame-locals frame-index thread)))
        (setf (debugger-state-locals state) (or locals nil)
              (debugger-state-locals-frame-index state) frame-index
              (debugger-state-view state) :locals
              (debugger-state-selected state) 0
              (debugger-state-scroll-offset state) 0)))))

(defun debugger-back-to-backtrace (state)
  "Return from :locals view to :backtrace view."
  (setf (debugger-state-view state) :backtrace
        (debugger-state-selected state) 0
        (debugger-state-scroll-offset state) 0))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Interactive Loop
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *debugger-state* nil
  "Current debugger state during interactive session.")

(defun run-debugger ()
  "Launch the interactive debugger TUI for the last error."
  (let* ((info *last-debug-info*)
         (frames (normalize-backtrace-frames (getf info :frames)))
         (*debugger-state* (make-debugger-state
                            :condition-type (or (getf info :condition-type) "ERROR")
                            :condition-message (or (getf info :condition-message) "")
                            :restarts (or (getf info :restarts) nil)
                            :frames (or frames nil)))
         (*debugger-lines-drawn* 0)
         (first-render t))
    (with-raw-mode
      (loop
        (render-debugger first-render)
        (setf first-render nil)
        (let ((key (read-key)))
          (cond
            ;; Quit: q, Esc, Ctrl-C
            ((or (eql key #\q) (eql key #\Q) (eql key :escape)
                 (eql key :interrupt) (eql key :eof))
             (return))
            ;; Navigate up
            ((or (eql key :up) (eql key #\k))
             (debugger-move :up))
            ;; Navigate down
            ((or (eql key :down) (eql key #\j))
             (debugger-move :down))
            ;; Enter: invoke restart (live mode only, restarts view)
            ((eql key :enter)
             (when (and (debugger-state-live-p *debugger-state*)
                        (eq (debugger-state-view *debugger-state*) :restarts)
                        (debugger-state-restarts *debugger-state*))
               (return (debugger-state-selected *debugger-state*))))
            ;; Tab: toggle view
            ((eql key :tab)
             (setf (debugger-state-view *debugger-state*)
                   (if (eq (debugger-state-view *debugger-state*) :backtrace)
                       :restarts :backtrace))
             (setf (debugger-state-selected *debugger-state*) 0)
             (setf (debugger-state-scroll-offset *debugger-state*) 0))))))))

(defun run-debugger-interactive (debug-event)
  "Launch interactive debugger TUI for a live debug session.
   DEBUG-EVENT is a plist with :thread :level :condition :restarts :frames.
   Returns :abort or a restart index (integer)."
  (let* ((condition (getf debug-event :condition))
         (raw-restarts (getf debug-event :restarts))
         (raw-frames (getf debug-event :frames))
         (thread (getf debug-event :thread))
         (level (getf debug-event :level))
         ;; condition is (message type-string [extra])
         (cond-message (if (listp condition) (first condition) ""))
         (cond-type (if (and (listp condition) (second condition))
                        ;; Strip brackets: "[Condition of type FOO]" -> "FOO"
                        (let ((type-str (second condition)))
                          (if (and (> (length type-str) 0)
                                   (char= (char type-str 0) #\[))
                              (let* ((prefix "Condition of type ")
                                     (pos (search prefix type-str)))
                                (if pos
                                    (string-right-trim '(#\] #\Space)
                                                       (subseq type-str (+ pos (length prefix))))
                                    type-str))
                              type-str))
                        "ERROR"))
         ;; restarts: ((name description) ...) - already in right format
         (restarts (if (listp raw-restarts) raw-restarts nil))
         ;; frames: ((number description [plist]) ...) -> normalize
         (frames (normalize-backtrace-frames raw-frames))
         (*debugger-state* (make-debugger-state
                            :condition-type cond-type
                            :condition-message (or cond-message "")
                            :restarts restarts
                            :frames (or frames nil)
                            :live-p t
                            :thread thread
                            :level level
                            :raw-frames (if (and (listp raw-frames)
                                                 (listp (first raw-frames)))
                                            raw-frames
                                            nil)))
         (*debugger-lines-drawn* 0)
         (first-render t))
    ;; Non-interactive (stdin is a pipe): auto-abort instead of showing TUI
    (unless (or *browser-terminal-active*
                #-windows (plusp (osicat-posix:isatty 0))
                #+windows t)
      (return-from run-debugger-interactive :abort))
    (with-raw-mode
      (loop
        (render-debugger first-render)
        (setf first-render nil)
        (let ((key (read-key)))
          (cond
            ;; Quit/abort
            ((or (eql key #\q) (eql key #\Q)
                 (eql key :interrupt) (eql key :eof))
             (return :abort))
            ;; Escape: back from locals, or abort
            ((eql key :escape)
             (if (eq (debugger-state-view *debugger-state*) :locals)
                 (debugger-back-to-backtrace *debugger-state*)
                 (return :abort)))
            ;; Backspace: back from locals
            ((eql key :backspace)
             (when (eq (debugger-state-view *debugger-state*) :locals)
               (debugger-back-to-backtrace *debugger-state*)))
            ;; Navigate up
            ((or (eql key :up) (eql key #\k))
             (debugger-move :up))
            ;; Navigate down
            ((or (eql key :down) (eql key #\j))
             (debugger-move :down))
            ;; Enter: context-dependent action
            ((eql key :enter)
             (case (debugger-state-view *debugger-state*)
               (:restarts
                (when (debugger-state-restarts *debugger-state*)
                  (let* ((idx (debugger-state-selected *debugger-state*))
                         (restart (nth idx (debugger-state-restarts *debugger-state*)))
                         (restart-name (first restart)))
                    (if (debugger-restart-needs-value-p restart-name)
                        (let ((value (debugger-prompt-restart-value)))
                          (when value
                            (return (cons idx value))))
                        (return idx)))))
               (:backtrace
                (debugger-switch-to-locals *debugger-state*))
               (:locals
                (debugger-inspect-frame-var *debugger-state*))))
            ;; Eval in frame
            ((eql key #\e)
             (when (debugger-state-thread *debugger-state*)
               (debugger-eval-in-frame *debugger-state*)))
            ;; Tab: toggle backtrace/restarts (not from locals)
            ((eql key :tab)
             (let ((view (debugger-state-view *debugger-state*)))
               (unless (eq view :locals)
                 (setf (debugger-state-view *debugger-state*)
                       (if (eq view :backtrace) :restarts :backtrace))
                 (setf (debugger-state-selected *debugger-state*) 0)
                 (setf (debugger-state-scroll-offset *debugger-state*) 0))))))))))
