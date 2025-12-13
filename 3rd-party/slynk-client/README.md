
# Slynk Client

Slynk Client is a Common Lisp implementation of the client side of the Slynk
debugging protocol used by [Sly](https://joaotavora.github.io/sly/), a [GNU
Emacs](https://www.gnu.org/software/emacs) mode that implements an IDE for Lisp
programming.
Emacs uses the Slynk protocol to communicate with a Lisp system
when a user runs the IDE, but the protocol is useful independently of Emacs
because it allows a client to evaluate expressions on a remote Lisp that's
running a Slynk server.

## The Slynk Client API

#### slime-connect host-name port &optional connection-closed-hook

```
Connects to the Slynk server running on HOST-NAME that is listening on PORT.
Returns a SLYNK-CONNECTION if the connection attempt is successful.  Otherwise,
returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime secret
file and there are network problems sending its contents to the remote Slynk
server.  If provided, function CONNECTION-CLOSED-HOOK is called when the
connection is closed.
```

#### slime-close connection

```
Closes CONNECTION to a Slynk server.
```

#### slime-eval sexp connection

```
Sends SEXP over CONNECTION to a Slynk server for evaluation and waits for the
result.  When the result is received, it is returned.  Signals
SLIME-NETWORK-ERROR when there are network problems sending SEXP.
```

#### slime-eval-async sexp connection &optional continuation

```
Sends SEXP over CONNECTION to a Slynk server for evaluation, then immediately
returns.  Some time later, after the evaluation is finished, CONTINUATION is
called with the result as argument.  Signals SLIME-NETWORK-ERROR when there are
network problems sending SEXP.
```

#### slime-migrate-evals old-connection new-connection

```
Evaluates on NEW-CONNECTION all the work pending on a closed OLD-CONNECTION.
Signals SLIME-NETWORK-ERROR when there are network problems.
```

#### slime-pending-evals-p connection

```
Returns T if there are outstanding evaluations pending on CONNECTION;
otherwise, returns NIL.
```

#### with-slime-connection (variable host-name port &optional connection-closed-hook) &body body

```
Wraps BODY in a LET form where VARIABLE is bound to the value returned by
(SLIME-CONNECT HOST-NAME PORT CONNECTION-CLOSED-HOOK).  Arranges for the Slynk
connection to be closed when control exits BODY.
```

For more information, see the documentation strings in
[slynk-client.lisp](https://gitlab.com/shookakko/slynk-client/-/blob/master/src/slynk-client.lisp)
and the example code in
[slynk-client-test.lisp](https://gitlab.com/shookakko/slynk-client/-/blob/master/tests/slynk-client-test.lisp).

## Slynk Client example

### Starting a Slynk server

The code below starts two Slynk servers, one listening on port 4005 and the
other listening on port 10000.

```
(load-quicklisp)
(asdf:load-system 'slynk)

(defvar *emacs-port* 4005)
(defvar *slynk-client-port* 10000)

(defun slynk-thread ()
  "Returns a thread that's acting as a Slynk server."
  (dolist (thread (sb-thread:list-all-threads))
    (when (com.google.base:prefixp "Slynk" (sb-thread:thread-name thread))
      (return thread))))

(defun wait-for-slynk-thread ()
  "Wait for the Slynk server thread to exit."
  (let ((slynk-thread (slynk-thread)))
    (when slynk-thread
      (sb-thread:join-thread slynk-thread))))

(defun main ()
  (setf slynk:*configure-emacs-indentation* nil
        slynk::*enable-event-history* nil
        slynk:*log-events* t)
  (slynk:create-server :port *emacs-port* :dont-close t)
  (slynk:create-server :port *slynk-client-port* :dont-close t)
  (wait-for-slynk-thread))

(main)
```

### Using Slynk Client to evaluate an expression on the server

Once the Slynk servers are running, you can connect to the server on port 4005
from Emacs using the command ```M-x slime-connect```.  This connection is a
normal Slime IDE session.  From the Slime IDE you can evaluate the following
code, which creates a Slynk Client connection to the server running on port
10000 and remotely evaluates the expression ```(cons 1 2)```.

```
(load-quicklisp)
(asdf:load-system 'slynk-client)

(slynk-client:with-slime-connection (connection "localhost" 10000)
  (slynk-client:slime-eval '(cons 1 2) connection))
```
