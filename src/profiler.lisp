;;; profiler.lisp - Flame graph profiling with Speedscope integration
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package :icl)

;;; Profile data storage
(defvar *profile-data* (make-hash-table :test 'equal)
  "Storage for profile data, keyed by profile ID.")

(defvar *profile-counter* 0
  "Counter for generating unique profile IDs.")

;;; Profiling functions - runs in backend via Slynk

(defun backend-profile-form (form-string &key (mode :cpu) (sample-interval 0.001) (max-samples 1000000))
  "Profile FORM-STRING execution in the backend and return folded stack string.
MODE can be :cpu (default), :alloc, or :time.
SAMPLE-INTERVAL is the sampling interval in seconds (default 0.001).
MAX-SAMPLES is the maximum number of samples to collect (default 1000000)."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; First, ensure sb-sprof is loaded in the backend
  (backend-eval-internal "(require 'sb-sprof)")
  ;; Now send profiling code - sb-sprof package exists so symbols can be read
  ;; This follows cl-flamegraph's approach with proper name extraction.
  ;; We run the profiled code in a separate thread to isolate profiler signals
  ;; from the Slynk server's socket operations (prevents SLIME-NETWORK-ERROR
  ;; when profiling in browser mode with multiple threads).
  (let* ((profile-code
           (format nil
                   "(labels ((get-name (obj)
                              ;; Extract clean function name like cl-flamegraph does
                              (typecase obj
                                (string obj)
                                (symbol (symbol-name obj))
                                (cons (let ((*print-pretty* nil)) (format nil \"~~S\" obj)))
                                (sb-di::compiled-debug-fun
                                 (get-name (slot-value obj 'sb-di::compiler-debug-fun)))
                                (sb-c::compiled-debug-fun
                                 (get-name (slot-value obj 'sb-c::name)))
                                (sb-kernel:code-component \"<code>\")
                                (t (princ-to-string obj)))))
                      ;; Run profiling in a separate thread to isolate SIGPROF signals
                      ;; from the Slynk server's socket operations
                      (let ((result-box (list nil))
                            (error-box (list nil)))
                        (let ((thread (sb-thread:make-thread
                                       (lambda ()
                                         (handler-case
                                             (let ((result nil))
                                               (sb-sprof:with-profiling (:mode ~S
                                                                         :sample-interval ~A
                                                                         :max-samples ~A
                                                                         :report nil)
                                                 (setf result (multiple-value-list ~A)))
                                               (setf (car result-box) result))
                                           (error (e)
                                             (setf (car error-box) (princ-to-string e)))))
                                       :name \"profiler-thread\")))
                          (sb-thread:join-thread thread))
                        (when (car error-box)
                          (error (car error-box)))
                        ;; Build call graph (required before map-traces)
                        (sb-sprof:report :stream (make-broadcast-stream))
                        ;; Generate folded stacks like cl-flamegraph does
                        ;; Build a tree like cl-flamegraph and count at each node
                        (let ((root (make-hash-table :test #'equal))
                              (trace-count 0))
                          (sb-sprof:map-traces
                           (lambda (thread trace)
                             (declare (ignore thread))
                             (incf trace-count)
                             (let ((current root))
                               (sb-sprof::map-trace-pc-locs
                                (lambda (info pc-or-offset)
                                  (declare (ignore pc-or-offset))
                                  (when info
                                    (let* ((name (get-name info))
                                           (child (gethash name current)))
                                      (unless child
                                        (setf child (cons 0 (make-hash-table :test #'equal)))
                                        (setf (gethash name current) child))
                                      (incf (car child))
                                      (setf current (cdr child)))))
                                trace)))
                           sb-sprof::*samples*)
                          ;; Convert tree to folded stacks
                          (with-output-to-string (s)
                            (format s \";; ~~D traces processed~~%\" trace-count)
                            (labels ((walk (node path)
                                       (maphash (lambda (name child)
                                                  (let ((new-path (append path (list name)))
                                                        (count (car child))
                                                        (children (cdr child)))
                                                    (format s \"~~{~~A~~^;~~} ~~D~~%\" new-path count)
                                                    (walk children new-path)))
                                                node)))
                              (walk root nil)))))))"
                   mode sample-interval max-samples form-string))
         (results (backend-eval-internal profile-code)))
    (if results
        (first results)
        "")))

(defun profile-has-samples-p (stacks)
  "Return T if STACKS (parsed stack list) contains actual profile samples."
  (and stacks (plusp (length stacks))))

;;; Frame filtering - remove Slynk/RPC overhead

(defun overhead-frame-p (name)
  "Return T if NAME appears to be Slynk/RPC/thread infrastructure overhead."
  (or
   ;; Foreign function calls (thread startup) - may have % prefix
   (search "foreign function" name)
   ;; FLET/LAMBDA wrappers (usually infrastructure)
   (and (plusp (length name))
        (char= (char name 0) #\())
   ;; Exact matches for common overhead frames
   (member name '("RUN" "EVAL" "SIMPLE-EVAL-IN-LEXENV" "EVAL-FOR-EMACS"
                  "CALL-WITH-BINDINGS" "CALL-WITH-BREAK-HOOK"
                  "INVOKE-WITH-SAVED-RESTRICTIONS")
           :test #'string=)
   ;; Prefix matches
   (some (lambda (prefix)
           (and (>= (length name) (length prefix))
                (string= name prefix :end1 (length prefix))))
         '("SLYNK" "SWANK" "SB-THREAD" "SB-UNIX" "SB-IMPL::" "SB-INT::"
           "SB-KERNEL::" "SB-VM::" "SB-C::" "SB-PCL::"
           "%WITH-" "CALL-WITH-SANE-IO-SYNTAX"))
   ;; Contains patterns (for embedded SLYNK references)
   (search "SLYNK" name)
   (search "SWANK" name)))

(defun user-code-frame-p (name)
  "Return T if NAME looks like user code rather than internal SBCL/thread overhead."
  (and name
       (plusp (length name))
       ;; Not a %-prefixed internal function
       (not (char= (char name 0) #\%))
       ;; Not a foreign function
       (not (search "foreign function" name))
       ;; Not a FLET/LAMBDA wrapper
       (not (char= (char name 0) #\())))

(defun trim-overhead-frames (stack)
  "Remove leading overhead frames from STACK, keeping from first non-overhead frame.
Returns NIL if the resulting stack doesn't look like user code."
  (let ((result stack))
    ;; Skip overhead frames at the start
    (loop while (and result (overhead-frame-p (first result)))
          do (pop result))
    ;; Only return if first remaining frame looks like user code
    ;; Otherwise this is probably an internal thread we should ignore
    (when (and result (user-code-frame-p (first result)))
      result)))

;;; Folded stacks to Speedscope JSON conversion

(defun parse-folded-stacks (folded-string)
  "Parse folded stack format into list of (stack-list . count) pairs.
Input format: 'FUNC1;FUNC2;FUNC3 count' per line.
Filters out Slynk/RPC overhead frames from stack starts."
  (let ((result nil))
    (with-input-from-string (stream folded-string)
      (loop for line = (read-line stream nil nil)
            while line
            when (plusp (length line))
              do (let ((space-pos (position #\Space line :from-end t)))
                   (when space-pos  ; Skip lines without space separator
                     (let* ((stack-str (subseq line 0 space-pos))
                            (count-str (subseq line (1+ space-pos)))
                            (count (parse-integer count-str :junk-allowed t)))
                       (when (and stack-str count (plusp count))
                         (let ((stack (trim-overhead-frames
                                       (split-sequence:split-sequence #\; stack-str))))
                           (when stack  ; Only add if stack has frames after trimming
                             (push (cons stack count) result)))))))))
    (nreverse result)))

(defun build-frame-index (stacks)
  "Build a frame name -> index mapping from stack data.
Returns (hash-table frames-vector)."
  (let ((frame-table (make-hash-table :test 'equal))
        (frames nil)
        (index 0))
    (dolist (stack-entry stacks)
      (dolist (frame (car stack-entry))
        (unless (gethash frame frame-table)
          (setf (gethash frame frame-table) index)
          (push frame frames)
          (incf index))))
    (values frame-table (coerce (nreverse frames) 'vector))))

(defun stacks-to-speedscope (stacks &optional (profile-name "Lisp Profile"))
  "Convert parsed stack data to Speedscope JSON string.
STACKS is a list of (stack-list . count) pairs."
  (let ((total-samples 0))
    ;; Calculate total samples
    (dolist (s stacks) (incf total-samples (cdr s)))

    (if (zerop total-samples)
        ;; Empty profile
        (com.inuoe.jzon:stringify
         (alexandria:plist-hash-table
          (list "$schema" "https://www.speedscope.app/file-format-schema.json"
                "shared" (alexandria:plist-hash-table
                          (list "frames" #()))
                "profiles" #()
                "name" profile-name)
          :test 'equal))
        ;; Build speedscope format
        (multiple-value-bind (frame-table frames-vec) (build-frame-index stacks)
          (let ((samples nil)
                (weights nil))
            ;; Build samples and weights arrays
            (dolist (stack-entry stacks)
              (let ((stack (car stack-entry))
                    (count (cdr stack-entry)))
                ;; Convert stack to frame indices
                (let ((indices (mapcar (lambda (f) (gethash f frame-table)) stack)))
                  (push (coerce indices 'vector) samples)
                  (push count weights))))
            ;; Build frames array
            (let ((frames-array
                    (map 'vector
                         (lambda (name)
                           (alexandria:plist-hash-table
                            (list "name" name)
                            :test 'equal))
                         frames-vec)))
              ;; Build the final structure
              (com.inuoe.jzon:stringify
               (alexandria:plist-hash-table
                (list "$schema" "https://www.speedscope.app/file-format-schema.json"
                      "shared" (alexandria:plist-hash-table
                                (list "frames" frames-array)
                                :test 'equal)
                      "profiles" (vector
                                  (alexandria:plist-hash-table
                                   (list "type" "sampled"
                                         "name" profile-name
                                         "unit" "samples"
                                         "startValue" 0
                                         "endValue" total-samples
                                         "samples" (coerce (nreverse samples) 'vector)
                                         "weights" (coerce (nreverse weights) 'vector))
                                   :test 'equal))
                      "name" profile-name)
                :test 'equal))))))))

(defun folded-to-speedscope (folded-string &optional (profile-name "Lisp Profile"))
  "Convert folded stack format to Speedscope JSON string."
  (stacks-to-speedscope (parse-folded-stacks folded-string) profile-name))

;;; Profile storage and retrieval

(defun store-profile (json-string)
  "Store profile data and return its ID."
  (let ((id (format nil "profile-~A-~A"
                    (incf *profile-counter*)
                    (get-universal-time))))
    (setf (gethash id *profile-data*) json-string)
    id))

(defun get-profile (id)
  "Retrieve stored profile data by ID."
  (gethash id *profile-data*))

(defun clear-old-profiles (&optional (max-age 3600))
  "Clear profiles older than MAX-AGE seconds."
  (let ((cutoff (- (get-universal-time) max-age)))
    (maphash (lambda (id data)
               (declare (ignore data))
               ;; Extract timestamp from ID
               (let* ((parts (split-sequence:split-sequence #\- id))
                      (timestamp (parse-integer (third parts) :junk-allowed t)))
                 (when (and timestamp (< timestamp cutoff))
                   (remhash id *profile-data*))))
             *profile-data*)))

;;; High-level profiling interface

(defun profile-and-store (form-string &key (mode :cpu) (name "Lisp Profile"))
  "Profile FORM-STRING in the backend, convert to Speedscope format, store and return profile ID.
Returns (values profile-id has-samples-p)."
  (let* ((folded (backend-profile-form form-string :mode mode))
         (stacks (parse-folded-stacks folded))
         (has-samples (profile-has-samples-p stacks))
         (json (stacks-to-speedscope stacks name))
         (id (store-profile json)))
    (values id has-samples)))
