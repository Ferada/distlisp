;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package :distlisp)

(defun wrap-local (message from)
  "Wrap a message into a tuple (<MESSAGE> . FROM) for local delivery."
  (cons message from))

(defun wrap-remote (to from message)
  "Wrap a message into a tuple (PIDTO PIDFROM <MESSAGE>) for remote delivery."
  `(,(pid-id to) ,(pid-id from) ,message))

(defun route-remote (to from message)
  (let ((node (pid-node to)))
    (when (valid-node? node)
      (write-node node (wrap-remote to from message))
      T)))

(defun route-local (to from message)
  (awhen (find-process to)
    (enqueue (mailbox-indeque (slot-value it 'mailbox))
	     (wrap-local message from))
    T))

(defun %send (to message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to another process."
  (if (pid-node to)
      (with-nodes (route-remote to from message))
      (with-processes (route-local to from message))))

(defun send-list (to from command &rest args)
  (%send to `(,command ,.args) from))

(defun send-node (node message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to the handler process at NODE."
  (with-nodes (route-remote (make-root-pid node) from message)))

;; sending a message verifying it using a unique token (number?)
#+noway
(defun send-command-verify ()
  (when (%send to `(:COMMAND ,token ,.args))
    (recv-if (lambda (message from)
	       (and (eq :RESPONSE (first message))
		    (eq token (second message)))))
    T))

;;; derived macros

(defmacro! send (to message)
  `(%send ,to ,message))
