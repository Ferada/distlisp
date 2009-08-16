;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun wrap-local (message from)
  (check-type from pid)
  "Wrap a message into a tuple (<MESSAGE> . FROM) for local delivery."
  (cons message from))

(defun wrap-remote (to from message)
  (check-type to pid)
  (check-type from pid)
  "Wrap a message into a tuple (PIDTO PIDFROM <MESSAGE>) for remote delivery."
  `(,(pid-id to) ,(pid-id from) ,message))

(defun route-remote (to from message)
  (check-type to pid)
  (check-type from pid)
  (let ((node (pid-node to)))
    (if (valid-node? node)
	(progn (write-node node (wrap-remote to from message))
	       T)
	(warn "remote process ~A on node ~A not valid, discarded"
	      (pid-id to) node))))

(defun route-local (to from message)
  (check-type to pid)
  (check-type from pid)
  (aif (find-process to)
       (progn (enqueue (mailbox-indeque (process-info-mailbox it))
		       (wrap-local message from))
	      T)
       (warn "local process ~A not found, discarded" (pid-id to))))

(defun %send (to message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to another process."
  (check-type to pid)
  (check-type from pid)
  (if (pid-node to)
      (with-processes (with-nodes (route-remote to from message)))
      (with-processes (route-local to from message))))

(defun send-node (node message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to the handler process at NODE."
  (check-type node node-info)
  (check-type from pid)
  (with-processes (with-nodes (route-remote (make-root-pid node) from message))))

;;; derived macros

;; (defmacro send (pid message-constructor))
