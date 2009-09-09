{comment -*- mode: markdown; mode: auto-fill; fill-column: 72; -*- }

# Distlisp Tutorial

## Preparations

First, let's create a new package for our sample session and load some libraries:

<code>
* (asdf:oos 'asdf:load-op 'distlisp)
* (defpackage :mytest
    (:use :cl :anaphora :fare-matcher :distlisp))
* (in-package :mytest)
</code>

Since distlisp already depends on those libraries, you should have no
problem loading them.

Next, we have to initialise the environment:

<code>
* (init-environment NIL)
</code>

The first parameter disables the server mode, which would allow other
nodes to connect to us.  After this call, we can spawn new processes and
send messages to them.

## Echo

<code>
* (defun start-echo ()
    (spawn (:local)
      (receive-loop (msg from)
        (send from msg)))))
* (defvar echo (start-echo))
</code>

START-ECHO spawns a new process and returns the PID to us.  This handle
can now be used to send messages to the process; likewise, we can
examine our message buffer and receive new messages:

<code>
* (send echo 'hello-world)
* (recv)
</code>

The RECV function has some variants to make life easier for us:

* RECV-NOWAIT doesn't block if no message is available,
* RECV-IF tests messages and returns only if a message matched some
  criteria,
* RECV-IF-NOWAIT does what you'd expect.

Additionally, a number of macros implement common usage patterns:

* RECEIVE-BIND is RECV with integrated MULTIPLE-VALUE-BIND for the
  message and the sender,
* RECEIVE-LOOP also binds message and sender and then loops a code block,
* RECEIVE is the all purpose macro with bells and whistles (integrates
  the (non-)blocking variants, timeout functionality and pattern
  matching using a pattern matching library (usally FARE-MATCHER).

## Process Death

Now, the echo server doesn't have a stop button.  Although it's not very
clean, KILL wraps (for the local case) a implementation dependant
function, which can kill a thread in a (somewhat) clean way.  You can
also use this function to send an exit signal to remote processes.

<code>
* (kill echo)
</code>

The optional parameter REASON can be used to indicate one important
case: if it's set to :KILL (the default), the process is killed, even if
it traps signals otherwise.
