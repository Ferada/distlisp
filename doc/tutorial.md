<!-- -*- mode: markdown; mode: auto-fill; fill-column: 72; -*- -->

# Distlisp Tutorial

This document contains some examples how to use the Distlisp library.
If you already know Erlang, you'll see similarities when some things
behave in a special way (e.g. process signals).

Other than that, you'll probably need good knowledge of Lisp.

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
send messages to them.

## Echo

Let's start with a very simple example, an echo server:

    * (defun start-echo ()
        (spawn (:local)
          (receive-loop (msg from)
            (send from msg)))))
    * (defvar echo (start-echo))

Spawn does the obvious thing, `:LOCAL` just specifies that we want a
local process (in contrast to `:REMOTE`).  There might be other cases,
but mostly, those are the useful ones for the time being.

`RECEIVE-LOOP` loops forever, MSG and FROM are the symbols to which the
values are bound during the enclosed block.

`START-ECHO` spawns a new process and returns the PID to us.  This
handle can now be used to send messages to the process; likewise, we can
examine our message buffer and receive new messages:

    * (send echo 'hello-world)
    * (recv)

The `RECV` function has some variants to make life easier for us:

* `RECV-NOWAIT` doesn't block if no message is available,
* `RECV-IF` tests messages and returns only if a message matched some
  criteria,
* `RECV-IF-NOWAIT` does what you'd expect.

Additionally, a number of macros implement common usage patterns:

* `RECEIVE-BIND` is `RECV` with integrated `MULTIPLE-VALUE-BIND` for the
  message and the sender,
* `RECEIVE-LOOP` also binds message and sender and then loops a code block,
* `RECEIVE` is the all purpose macro with bells and whistles (integrates
  the (non-)blocking variants, timeout functionality and pattern
  matching using a pattern matching library (usally `FARE-MATCHER`).

## Process Death

Now, the echo server doesn't have a stop button.  Although it's not very
clean, `KILL` wraps (for the local case) a implementation dependant
function, which can kill a thread in a (somewhat) clean way.  You can
also use this function to send an exit signal to remote processes.

    * (kill echo)

The optional parameter `REASON` can be used to indicate one important
case: if it's set to `:KILL` (the default), the process is killed, even if
it traps signals otherwise.
