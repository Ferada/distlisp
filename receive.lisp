;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun current-indeque ()
  (mailbox-indeque (process-info-mailbox *current-process*)))

;;; receiving messages variants

(defun %receive (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Blocks if none available."
  (dequeue-wait deque))

(defun %receive-nowait (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Returns NIL if there's no message."
  (dequeue deque))

(defun %receive-if (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Blocks if none available."
  (dequeue-wait-if deque (lambda (message) (funcall test (car message) (cdr message)))))

(defun %receive-if-nowait (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Returns NIL if there's no message."
  (dequeue-if deque (lambda (message) (funcall test (car message) (cdr message)))))

;;; derived macros

(defmacro! receive-bind ((&optional (msg 'MSG) (from 'FROM)) &body body)
  "Receives a message, binding it to the variable in MSG, default MSG and
the sender to the symbol in FROM, default FROM."
  `(multiple-value-bind (,msg ,from) (%receive)
     ,.body))

(defmacro! receive-loop ((&optional (msg 'MSG) (from 'FROM)) &body body)
  `(loop as ,g!recv = (%receive)
      as ,msg = (car ,g!recv)
      and ,from = (cdr ,g!recv)
      do (progn ,.body)))
