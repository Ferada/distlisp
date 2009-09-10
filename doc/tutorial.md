<!-- -*- mode: markdown; mode: auto-fill; fill-column: 72; -*- -->

# Distlisp Tutorial

This document contains some examples on how to use the Distlisp library.
If you already know Erlang, you'll see similarities when some things
behave in a special way (e.g. process signals, linking and monitoring).

Other than that, you'll probably need a fair knowledge of Lisp.

## Preparations

First, let's create a new package for our sample session and load some libraries:

    * (in-package :cl-user)
    * (asdf:oos 'asdf:load-op 'distlisp)
    * (defpackage :mytest
        (:use :cl :anaphora :fare-matcher :distlisp))
    * (in-package :mytest)

Since Distlisp already depends on those libraries, you should have no
problem loading them.

Next, we have to initialise the environment:

    * (init-environment NIL)

The first parameter disables the server mode, which would allow other
nodes to connect to us.  After this call, we can spawn new processes and
send messages to them.  As a sidenote, you can only register one thread
using this function, since it uses a global variable to hold the ID of
the current thread, which is then rebound in newly spawned threads.

A PID is a tuple of a node object (or NIL in the case of the local node)
and a number, which identifies the process on the node.  Since the IDs
are therefore values relative to our current node, they should just be
opaque objects for you: sending them as messages has no meaning between
nodes and at the moment, there's no position independent encoding for
nodes or PIDs (but it'll be added soon enough, since it is needed for
more than two nodes anyway).

## Echo Server

Let's start with a very simple example, an echo server:

    * (defun start-echo ()
        (spawn (:local)
          (receive-loop (msg from)
            (send from msg)))))
    * (defvar echo (start-echo))

Spawn does the obvious thing, `:LOCAL` just specifies that we want a
local process (in contrast to `:REMOTE`).  There might be other options,
but mostly, those are the two useful ones for the time being.

`RECEIVE-LOOP` loops forever receiving messages; MSG and FROM are the
symbols to which the values are bound during the enclosed block.

When invoked, `START-ECHO` spawns a new process and returns the PID to
us.  This handle can now be used to send messages to the process;
likewise, we can examine our own message buffer and receive new
messages:

    * (send echo 'hello-world)
    * (recv)

The `RECV` function has some variants to make life easier for us:

* `RECV-NOWAIT` doesn't block if no message is available,
* `RECV-IF` tests messages and returns only if a message matched some
  criteria,
* `RECV-IF-NOWAIT` does what you'd expect.

Note that all of them return multiple values: the first is the received
message, the second is the sender PID.

Additionally, a number of macros implement common usage patterns:

* `RECEIVE-BIND` is `RECV` with integrated `MULTIPLE-VALUE-BIND` for the
  message and the sender,
* `RECEIVE-LOOP` also binds message and sender and then loops a code
  block,

Both macros provide optional arguments to change the symbol the message
and the sender are bound in the supplied body.  By default, both intern
`MSG` and `FROM` in the current package.

* `RECEIVE` is the all purpose macro with bells and whistles (integrates
  the (non-)blocking variants, timeout functionality (using
  `TRIVIAL-TIMEOUT`) and pattern matching using a decided library
  (usally `FARE-MATCHER`, but I try to keep this tweakable).

`RECEIVE` doesn't have syntax for matching the sender of a message
(mostly because it'd look ugly and also because PIDs should be regarded
as opaque objects), but has a parameter to bind it to a user
configurable symbol.  I'll be mostly using this macro in the next
parts.  Erlang style `when` guards should be and are (in case of
`FARE-MATCHER` with `LIKE-WHEN`) provided by the matcher library.

### Sending Messages

The `SEND` macro does not fail on purpose.  It returns `T` as long, as
it has knowledge of the receiver at the current time.  If it doesn't, a
warning is generated and `NIL` is returned.

Obviously, only the return value `NIL` guarantees, that the message
*wasn't* send; the return value `T` doesn't prove anything at all.

### Process Death

Now, the echo server doesn't have a stop button.  Although it's not very
clean, `KILL` wraps (for the local case) a implementation dependant
function, which can kill a thread in a (somewhat) clean way.  You can
also use this function to send an exit signal to remote processes.

    * (kill echo)

The optional parameter `REASON` can be used to indicate one important
case: if it's set to `:KILL` (the default), the process is killed, even if
it traps signals otherwise.

## Dictionary Server

Although it's probably not really idiomatic (Common) Lisp style, the
next example uses only non-destructive / functional operations and
recursion.  If a particular implementation doesn't provide tail
recursion the stack will overflow at some point, so this style should
only be used for educational purposes and for comparison with an
equivalent Erlang solution.

    * (defun dict (list)
        (receive (:from sender)
          (`(GET ,key)
            (aif (assoc key list)
                 (send sender `(OK ,it))
                 (send sender 'ERROR))
            (dict list))
          (`(SET ,key ,value)
            (send sender 'OK)
            (let ((new `(,key . ,value)))
              (dict (aif (assoc key list)
                         (substitute new it list)
                         (cons new list)))))
          (`(REMOVE ,key)
            (send sender 'OK)
            (dict (remove key list :key #'car)))
          ('QUIT
            (send sender 'OK))
          (other
            (send sender 'ERROR)
            (dict list))))

    * (defun start-dict (&optional list)
        (spawn ()
          (dict list)))

    * (recv-nowait)
    > NIL

    * (defvar dict (start-dict '((:FOO . 1) (:BAR . 2))))

    * (send dict '(GET :FOO))
    * (recv)
    > (OK (:FOO . 1))

    * (send dict '(SET :QUX 42))
    * (send dict '(GET :QUX))
    * (recv)
    > OK
    * (recv)
    > (OK (:QUX . 42))

    * (send dict '(REMOVE :QUX))
    * (send dict '(GET :QUX))
    * (recv)
    > OK
    * (recv)
    > ERROR

    * (send dict 'BLUB)
    * (recv)
    > ERROR

    * (send dict 'QUIT)
    * (recv)
    > OK

    * (send dict 'QUIT)
    > WARNING: local process 4 not found, discarded
    > NIL
