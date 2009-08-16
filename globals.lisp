;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defvar *processes* NIL
  "List of running processes.")

(defvar *processes-lock* (make-lock)
  "Global lock for processes.")

(defmacro with-processes (&body body)
  "Executes BODY with *PROCESSES-LOCK* held."
  `(with-lock-held (*processes-lock*)
     ,@body))

(defvar *nodes* NIL
  "List of connected nodes.")

(defvar *nodes-lock* (make-lock)
  "Global lock for nodes.")

(defmacro with-nodes (&body body)
  "Executes BODY with *NODES-LOCK* held."
  `(with-lock-held (*nodes-lock*)
     ,@body))

;;; some shortcuts and thread-local variables

(defvar* *current-process*
  "Thread-local process variable.")

(defvar* *current-pid*
  "Thread-local process ID.")

(defvar *services* NIL
  "A list of externally visible, registered processes.")

(defvar *services-lock* (make-lock)
  "The lock for *SERVICES*.")

(defmacro with-services (&body body)
  "Executes BODY with *SERVICES-LOCK* held."
  `(with-lock-held (*services-lock*)
     ,@body))
