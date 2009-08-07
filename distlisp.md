{comment -*- mode: markdown; mode: auto-fill; fill-column: 72; -*- }

# Distlisp

## Overview

Distlisp is a very simple package to to message passing in Common Lisp.

The basic API just handles the communication inside of one node, that
is, one Lisp application.  Using {package bordeaux-threads} multiple
threads can send messages to each other and spawn new threads.

The extended API adds communication between nodes over network using
{package iolib}.  The serialisation package is replaceable (by default
the Lisp printer and {package cl-store} are supported.  Negotiation of
the used package is supported via a short handshake protocol on each
connection.  Connection encryption is currently supported via {package
cl+ssl}.

Additional error prevention can be added via message counters for
inividual messages.

Using the services API, special

## Dependencies

Distlisp depends on a small number of core packages:

- logv
- utils-frahm
- anaphora
- fare-matcher
- bordeaux-threads

Additionally, the following packages are required for additional
functionality:

- cl+ssl
- cl-store

## Coding Conventions

Helper functions, which are used in more general functions with the same
intended functionality are marked with a prefixed percentage (`%`) symbol.

Predicates are suffixed with a question mark rather than `p`.

The implementation uses some shortcuts without message passing via
global variables and locking.

Library errors are signaled using the normal error handling facilities
of Common Lisp; errors coming from other nodes can either be converted
to normal messages or can be signaled on receiving (e.g. process
death).

## Basic API

Although in-memory message passing doesn't use serialisation, you
shouldn't send possibly unserialisable objects, like closures, if you
later want to extend your application to multiple nodes.

The global variable {var *processes*} contains all local processes with
access to the message passing functionality.  It is only accessed with
the lock {var *processes-lock*} held using {macro with-processes}.

Normal threads can be registered using {fun REGISTER-CURRENT-THREAD}.

## Message Protocol

There are two seperate protocols similiar to the two APIs.

The first one is the local intra-node protocol.

Every message here is wrapped in a cons cell, with the {fun car}
containing the actual message and the {fun cdr} the sender PID, which
can be accessed with the receive functions.

The second protocol is for inter-node communication.

Here, every message is wrapped inside a list like follows: the first
element is the target PID, the second the sender PID and the third the
actual message.

This packages are unpacked in the reader thread for every connection,
rewritten with the local node information and then handled like normal
intra-node messages.

`:NOPROCESS` is send back if for some message the corresponding process
couldn't be found.  The node handler is responsible for handling this
message.

`:QUIT` terminates the connection between two nodes, no further messages
are send.

`:SPAWN` is used to create a new process on a remote node.  The answer
is `:SPAWNED`.
