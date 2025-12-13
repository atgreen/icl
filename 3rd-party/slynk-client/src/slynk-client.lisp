(in-package #:slynk-client)

(deftype port () "A non-privileged TCP/IP port number." '(integer 1024 65535))

(define-constant +abort+ (cons nil nil)
  :documentation
  "Unique object used to signal that a computation was aborted on the server."
  :test #'equal)

(defvar *thread-offset* 0
  "Counter used to assign each Slynk connection a unique range of thread IDs.")

(define-constant +maximum-thread-count+ 10000
  :documentation
  "Maximum number of threads per Slynk connection."
  :test #'=)

(define-condition slime-network-error (error)
  ()
  (:documentation "Network problem while evaluating a form."))

(defclass slynk-connection ()
  ((host-name :reader host-name
              :type string
              :initarg :host-name
              :documentation "Name of the host where the Slynk server is running.")
   (port :reader port
         :type port
         :initarg :port
         :documentation "Port number used to make a Slynk server connection.")
   (usocket :reader usocket
            :type usocket:stream-usocket
            :initarg :usocket
            :documentation "USOCKET used to communicate with the Slynk server.")
   (thread-offset :reader thread-offset
                  :initform (incf *thread-offset* +maximum-thread-count+)
                  :type (integer 0 *)
                  :documentation
		  "All threads for this connection are presented to Emacs with this value added to
their thread ID.")
   (continuation-counter :accessor continuation-counter
                         :initform 0
                         :type (integer 0 *)
                         :documentation "Used to associate an ID with each evaluated form.")
   (rex-continuations :accessor rex-continuations
                      :initform '()
                      :type list
                      :documentation
		      "List of (ID, continuation) pairs, one for each evaluation in progress. Used to
match each returned value with the continuation it should be passed to.")
   (state :accessor state
          :initform :alive
          :type (member :alive :closing :dead)
          :documentation "State of the connection, either :ALIVE, :CLOSING, or :DEAD.")
   (connection-lock :reader connection-lock
                    :initform (bordeaux-threads:make-lock)
                    :documentation
		    "Lock protecting slots of this connection that are read and written by
concurrently running threads."))
  (:documentation "A connection to a Slynk server."))

(defvar *open-connections* '()
  "List of all open Slynk connections.")

(defvar *connections-lock* (bordeaux-threads:make-lock)
  "Lock protecting *OPEN-CONNECTIONS*.")
(defgeneric add-open-connection (connection)
  (:documentation
   "Adds CONNECTION to the set of open Slynk connections."))

(defmethod add-open-connection ((connection slynk-connection))
  (bordeaux-threads:with-lock-held (*connections-lock*)
    (push connection *open-connections*)))

(defgeneric remove-open-connection (conection)
  (:documentation
   "Removes CONNECTION from the set of open Slynk connections."))

(defmethod remove-open-connection ((connection slynk-connection))
  (bordeaux-threads:with-lock-held (*connections-lock*)
    (setf *open-connections* (remove connection *open-connections*))))

(defun find-connection-for-thread-id (thread-id)
  "Returns the open Slynk connection associated with THREAD-ID."
  (bordeaux-threads:with-lock-held (*connections-lock*)
    (let ((thread-offset (* (floor thread-id +maximum-thread-count+)
			    +maximum-thread-count+)))
      (find thread-offset *open-connections* :key #'thread-offset))))

(defun server-thread-id (thread-id)
  "Maps the THREAD-ID in an event that must be forwarded to the thread ID known
by the remote Lisp to which it will be sent."
  (mod thread-id +maximum-thread-count+))

(defun forward-event-to-worker (form package thread-id id)
  "Determines whether an :emacs-rex event is intended for a remote worker Lisp
and if so forwards it.  When forwarding is successful, FORWARD-EVENT-TO-WORKER
returns T; otherwise, it returns NIL.

FORWARD-EVENT-TO-WORKER is called by code in Slynk Crew's patch to Slime's
slynk.lisp source file.  The forwarding it performs is used by Slynk Crew
to handle debugging of conditions signalled on remote worker Lisps.  See
slynk.lisp-patch in https://github.com/brown/slynk-crew."
  (let ((connection (find-connection-for-thread-id thread-id)))
    (when connection
      (let ((remote-thread-id (server-thread-id thread-id)))
        (slime-send `(:emacs-rex ,form ,package ,remote-thread-id ,id) connection))
      t)))

(defvar *io-package*
  (let ((package (make-package :slynk-client-io-package :use '())))
    (import '(nil t quote) package)
    package)
  "A package used by the Slynk client code when printing s-expressions, so that
symbols in the printed output contain their package names.")

(defun slime-net-encode-length (n)
  "Encodes an integer as a 6-character, 24-bit hex string."
  (format nil "~6,'0,X" n))

(defun slime-net-send (sexp usocket)
  "Sends SEXP to a Slynk server over USOCKET.  The s-expression is read and
evaluated by the remote Lisp."
  (let* ((payload (with-standard-io-syntax
		    (let ((*package* *io-package*))
		      (prin1-to-string sexp))))
         (utf8-payload (string-to-utf8-octets payload))
         ;; The payload always includes one more octet, an encoded newline character at the end.
         (payload-length (1+ (length utf8-payload)))
         (utf8-length (string-to-utf8-octets (slime-net-encode-length payload-length)))
         ;; The encoded length always takes 6 octets.
         (message (make-octet-vector (+ (length utf8-length) payload-length))))
    (replace message utf8-length)
    (replace message utf8-payload :start1 (length utf8-length))
    (setf (aref message (1- (length message))) (char-code #\Newline))
    ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
    ;; system-dependent network or stream errors.
    (let ((success (ignore-errors
		    (write-sequence message (usocket:socket-stream usocket)))))
      (unless success (error 'slime-network-error)))))

(defun slime-send (sexp connection)
  "Sends SEXP to a Slynk server using CONNECTION.  Signals SLIME-NETWORK-ERROR
if there are communications problems."
  (let ((usocket (usocket connection)))
    (slime-net-send sexp usocket)
    ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
    ;; system-dependent network or stream errors.
    (let ((success nil))
      (ignore-errors
       (progn (force-output (usocket:socket-stream usocket))
	      (setf success t)))
      (unless success (error 'slime-network-error))))
  (values))

(defun socket-keep-alive (socket)
  "Configures TCP keep alive packets for SOCKET.  The socket connection will be
considered dead if keep alive packets are lost."
  (declare (ignorable socket))
  #+allegro
  (socket:set-socket-options socket :keepalive t)
  #+ccl
  (ccl::set-socket-options socket :keepalive t)
  #+sbcl
  (setf (sb-bsd-sockets:sockopt-keep-alive socket) t)
  #+(and linux sbcl)
  (setf (sb-bsd-sockets:sockopt-tcp-keepcnt socket) 1
        (sb-bsd-sockets:sockopt-tcp-keepidle socket) 30
        (sb-bsd-sockets:sockopt-tcp-keepintvl socket) 30))

(defun slime-net-connect (host-name port)
  "Establishes a connection to the Slynk server listening on PORT of HOST-NAME.
Returns a SLYNK-CONNECTION when the connection attempt is successful.
Otherwise, returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime
secret file and there are network problems sending its contents to the remote
Slynk server."
  (let ((usocket (handler-case
		     (usocket:socket-connect host-name port :element-type 'octet)
                   (usocket:socket-error ()
		     (return-from slime-net-connect nil)))))
    (socket-keep-alive (usocket:socket usocket))
    (let ((connection
	    (make-instance
	     'slynk-connection :host-name host-name :port port :usocket usocket)))
      connection)))

;;TODO: Evaluate the real value of this function
(defun send-to-emacs (event)
  "Sends EVENT to Emacs."
  (print event)
  ;;(slynk::send (slynk::mconn.control-thread (slynk::default-connection)) event)
  )

;;;; Protocol event handler (the guts)

;;; This is the protocol in all its glory. The input to this function is a protocol event that
;;; either originates within Emacs or arrived over the network from Lisp.
;;;
;;; Each event is a list beginning with a keyword and followed by arguments. The keyword identifies
;;; the type of event. Events originating from Emacs have names starting with :emacs- and events
;;; from Lisp don't.

(defun slime-dispatch-event (event connection)
  "Handles EVENT for a Slynk CONNECTION.  Signals SLIME-NETWORK-ERROR if there
are communications problems."
  (destructure-case event
    ((:emacs-rex form package-name thread continuation)
     (let ((id nil))
       (bordeaux-threads:with-lock-held ((connection-lock connection))
	 (setf id (incf (continuation-counter connection)))
	 (push (list id continuation form package-name thread) (rex-continuations connection))
	 (when (eq (state connection) :dead) (error 'slime-network-error)))
       (let ((name (format nil "slynk sender for ~A/~D" (host-name connection) (port connection))))
	 (bordeaux-threads:make-thread
	  (lambda ()
	    ;; Catch network errors so the Slynk sender thread exits gracefully if there are
	    ;; communications problems with the remote Lisp.
	    (handler-case
		(slime-send `(:emacs-rex ,form ,package-name ,thread ,id) connection)
	      (slime-network-error ())))
	  :name name))))
    ((:return value id)
     (let ((send-to-emacs t))
       (bordeaux-threads:with-lock-held ((connection-lock connection))
	 (let ((rec (assoc id (rex-continuations connection))))
	   (when rec
	     (setf send-to-emacs nil)
	     (setf (rex-continuations connection) (remove rec (rex-continuations connection)))
	     (funcall (second rec) value))))
       ;; The value returned is not for us.  Forward it to Slime.
       (when send-to-emacs
	 (force-output)
	 (send-to-emacs `(:return ,(slynk::current-thread) ,value ,id)))))

    ;; When a remote computation signals a condition and control ends up in the debugger, Slynk
    ;; sends these events back to pop up a Slime breakpoint window.  Forward the events to Slime.
    ;; Modify the thread ID of each event to uniquely identify which remote Lisp generated it.
    ((:debug-activate thread level &optional select)
     (incf thread (thread-offset connection))
     (send-to-emacs `(:debug-activate ,thread ,level ,select)))
    ((:debug thread level condition restarts frames continuations)
     (incf thread (thread-offset connection))
     ;;TODO: Change this there is no Emacs here
     (send-to-emacs `(:debug ,thread ,level ,condition ,restarts ,frames ,continuations)))
    ((:debug-return thread level stepping)
     (incf thread (thread-offset connection))
     (send-to-emacs `(:debug-return ,thread ,level ,stepping)))

    ((:emacs-interrupt thread)
     (slime-send `(:emacs-interrupt ,thread) connection))
    ((:channel-send id msg)
     (print (list :channel-send id msg)))
    ((:emacs-channel-send id msg)
     (slime-send `(:emacs-channel-send ,id ,msg) connection))
    ((:read-from-minibuffer thread tag prompt initial-value)
     (print (list :read-from-minibuffer thread tag prompt initial-value)))
    ((:y-or-n-p thread tag question)
     (print (list :y-or-n-p thread tag question)))
    ((:emacs-return-string thread tag string)
     (slime-send `(:emacs-return-string ,thread ,tag ,string) connection))
    ;; Ignore remote Lisp feature changes.
    ((:new-features features)
     (declare (ignore features)))
    ;; Ignore remote Lisp indentation updates.
    ((:indentation-update info)
     (declare (ignore info)))
    ((:eval-no-wait form)
     (print (list :eval-no-wait form)))
    ((:eval thread tag form-string)
     (print (list :eval thread tag form-string)))
    ((:ed-rpc-no-wait function-name &rest args)
     (print (list :ed-rpc-no-wait function-name '&rest args)))
    ((:ed-rpc thread tag function-name &rest args)
     (print (list :ed-rpc thread tag function-name '&rest args)))
    ((:emacs-return thread tag value)
     (slime-send `(:emacs-return ,thread ,tag ,value) connection))
    ((:ed what)
     (print (list :ed what)))
    ((:inspect what wait-thread wait-tag)
     (print (list :inspect what wait-thread wait-tag)))
    ((:background-message message)
     (print (list :background-message message)))
    ((:debug-condition thread message)
     (assert thread)
     (print (list :debug-condition thread message)))
    ((:ping thread tag)
     (slime-send `(:emacs-pong ,thread ,tag) connection))
    ((:reader-error packet condition)
     (print (list :reader-error packet condition))
     (error "Invalid protocol message"))
    ((:invalid-rpc id message)
     (setf (rex-continuations connection) (remove id (rex-continuations connection) :key #'car))
     (error "Invalid rpc: ~S" message))
    ((:emacs-skipped-packet packet)
     (print (list :emacs-skipped-packet packet)))
    (t
     (error "Unknown event received: ~S" event))))

(defun slime-net-read (connection)
  "Reads a Slynk message from a network CONNECTION to a Slynk server.  Returns
the Slynk event or NIL, if there was a problem reading data."
  (flet ((safe-read-sequence (buffer stream)
	   ;; We use IGNORE-ERRORS here to catch SB-INT:CLOSED-STREAM-ERROR on SBCL and any other
	   ;; system-dependent network or stream errors.
	   (let ((result (ignore-errors (read-sequence buffer stream))))
	     (unless result (return-from slime-net-read))
	     result)))
    (let ((stream (usocket:socket-stream (usocket connection)))
	  (length-buffer (make-octet-vector 6)))
      (if (/= (safe-read-sequence length-buffer stream) 6)
	  nil
	  (let* ((length-string (utf8-octets-to-string length-buffer))
		 (length (parse-integer length-string :radix 16))
		 (message-buffer (make-octet-vector length)))
	    (if (/= (safe-read-sequence message-buffer stream) length)
		nil
                (let ((message (utf8-octets-to-string message-buffer)))
                  (with-standard-io-syntax
		    (let ((*package* *io-package*))
		      (read-from-string message))))))))))

(defmacro slime-rex ((&rest saved-vars) (sexp connection) &body continuations)
  "(slime-rex (VAR ...) (SEXP CONNECTION) CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each VAR is
either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the PRINCed version is sent over CONNECTION to a remote
Lisp.

CLAUSES is a list of patterns with same syntax as `destructure-case'.  The
result of the evaluation of SEXP is dispatched on CLAUSES.  The result is either
a sexp of the form (:ok VALUE) or (:abort CONDITION).  CLAUSES is executed
asynchronously.

Signals SLIME-NETWORK-ERROR when there are network problems sending SEXP."
  (let ((result (gensym)))
    `(let ,(loop for var in saved-vars
                 collect (etypecase var
			   (symbol (list var var))
			   (cons var)))
       (slime-dispatch-event (list :emacs-rex
                                   ,sexp
                                   "COMMON-LISP-USER"
                                   t
                                   (lambda (,result)
                                     (destructure-case ,result ,@continuations)))
                             ,connection))))

(defun slime-eval-async (sexp connection &optional continuation)
  "Sends SEXP over CONNECTION to a Slynk server for evaluation, then immediately
returns.  Some time later, after the evaluation is finished, CONTINUATION is
called with the result as argument.  Signals SLIME-NETWORK-ERROR when there are
network problems sending SEXP."
  (slime-rex (continuation)
      (sexp connection)
    ((:ok result)
     (when continuation
       (funcall continuation result)))
    ((:abort condition)
     (when continuation
       (funcall continuation (cons +abort+ condition)))))
  (values))

(defun slime-eval (sexp connection)
  "Sends SEXP over CONNECTION to a Slynk server for evaluation and waits for the
result.  When the result is received, it is returned.  Signals
SLIME-NETWORK-ERROR when there are network problems sending SEXP."
  (let* ((done-lock (bordeaux-threads:make-lock "slime eval"))
         (done (bordeaux-threads:make-condition-variable))
         (result-available nil)
         (result nil))
    ;; See the Bordeaux Threads documentation for a description of the locking pattern used here.
    (slime-eval-async sexp
                      connection
                      (lambda (x)
                        (bordeaux-threads:with-lock-held (done-lock)
			  (setf result x
				result-available t)
			  (bordeaux-threads:condition-notify done))))
    (bordeaux-threads:with-lock-held (done-lock)
      ;; Do not call CONDITION-WAIT if our result is already available, since we would wait forever
      ;; on the DONE condition variable, which has already been notified.  Also, CONDITION-WAIT can
      ;; return spuriously before DONE has been notified, so wait again if our result is not yet
      ;; available.
      (loop until result-available
	    do (bordeaux-threads:condition-wait done done-lock)))
    (when (and (consp result) (eq (car result) +abort+))
      (error "Evaluation aborted on ~s." (cdr result)))
    result))

(defun slime-pending-evals-p (connection)
  "Returns T if there are outstanding evaluations pending on CONNECTION;
otherwise, returns NIL."
  (not (null (rex-continuations connection))))

(defun slime-migrate-evals (old-connection new-connection)
  "Evaluates on NEW-CONNECTION all the work pending on a closed OLD-CONNECTION.
Signals SLIME-NETWORK-ERROR when there are network problems."
  (dolist (rec (rex-continuations old-connection))
    (destructuring-bind (id continuation form package-name thread)
	rec
      (declare (ignore id))
      (slime-dispatch-event `(:emacs-rex ,form ,package-name ,thread ,continuation)
			    new-connection)))
  (setf (rex-continuations old-connection) '()))

(defun slime-dispatch-events (connection connection-closed-hook)
  "Reads and dispatches incoming events for a CONNECTION to a Slynk server.  If
provided, function CONNECTION-CLOSED-HOOK is called when CONNECTION is closed."
  (flet ((close-connection ()
	   (bordeaux-threads:with-lock-held ((connection-lock connection))
	     (usocket:socket-close (usocket connection))
	     (setf (state connection) :dead))
	   (remove-open-connection connection)
	   (when connection-closed-hook (funcall connection-closed-hook))))
    (loop (let ((event (slime-net-read connection)))
	    (unless event
	      (close-connection)
	      (return-from slime-dispatch-events))
	    ;; TODO(brown): Verify that this call to SLIME-DISPATCH-EVENTS will never signal
	    ;; SLIME-NETWORK-ERROR.
	    (slime-dispatch-event event connection))
	  (let ((state nil))
	    (bordeaux-threads:with-lock-held ((connection-lock connection))
	      (setf state (state connection)))
	    (ecase state
	      (:alive)
	      (:closing
	       (close-connection)
	       (return-from slime-dispatch-events))
	      (:dead
	       (return-from slime-dispatch-events)))))))

(defun slime-connect (host-name port &optional connection-closed-hook)
  "Connects to the Slynk server running on HOST-NAME that is listening on PORT.
Returns a SLYNK-CONNECTION if the connection attempt is successful.  Otherwise,
returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime secret file
and there are network problems sending its contents to the remote Slynk server.
If provided, function CONNECTION-CLOSED-HOOK is called when the connection is
closed."
  (let ((connection (slime-net-connect host-name port)))
    (when connection
      (add-open-connection connection)
      ;; Create a thread to handle incoming events from the remote Lisp.
      (let ((name (format nil "slynk dispatcher for ~A/~D" host-name port)))
        (bordeaux-threads:make-thread (lambda ()
                                        (slime-dispatch-events connection connection-closed-hook))
                                      :name name)))
    connection))

(defun slime-close (connection)
  "Closes CONNECTION to a Slynk server."
  (bordeaux-threads:with-lock-held ((connection-lock connection))
    (setf (state connection) :closing))
  (slime-eval-async nil connection)
  (values))

(defmacro with-slime-connection ((variable host-name port &optional connection-closed-hook)
                                 &body body)
  "Wraps BODY in a LET form where VARIABLE is bound to the value returned by
(SLIME-CONNECT HOST-NAME PORT CONNECTION-CLOSED-HOOK).  Arranges for the Slynk
connection to be closed when control exits BODY."
  `(let ((,variable (slime-connect ,host-name ,port ,connection-closed-hook)))
     (unless ,variable (error 'slime-network-error))
     (unwind-protect
	  (progn ,@body)
       (slime-close ,variable))))
