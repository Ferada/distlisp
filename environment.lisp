;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun register-current-thread ()
  (with-processes
    (unless (member (current-thread) *processes*
		    :key (lambda (process) (when (typep process 'thread-process-info)
					     (slot-value process 'thread)))
		    :test #'eq)
      (let* ((pid (make-pid :id (pid-counter-next pid-counter)))
	     (info (make-instance 'thread-process-info
				  :pid pid
				  :thread (current-thread)
				  ;; you wouldn't like it to be different
				  :traps-exit T)))
	(setf *current-process* info
	      *current-pid* pid)
	(add-process info)))))

(defun stop-environment (&optional (timeout default-timeout))
  (flet ((kill-nodes ()
	   (with-nodes (mapcar #'kill-node *nodes*)))
	 (kill-processes ()
	   (mapcar #'kill-process *processes*)))
    (when (root-alivep)
      (kill-root))
    (with-processes
      (kill-nodes)
      (kill-processes))
    (when (with-processes (or *processes* (with-nodes *nodes*)))
      (sleep timeout)
      (with-processes
	(kill-nodes)
	(kill-processes)))
    (when (with-processes *processes*)
      (warn "some processes still alive"))
    (when (with-processes (with-nodes *nodes*))
      (warn "some nodes still connected"))))

(defun init-environment (&optional (start-root T) (timeout default-timeout))
  (with-processes (register-current-thread))
  (unless start-root
    (return-from init-environment))
  (when (root-alivep)
    (kill-root))
  (sleep 0.1)
  (when (root-alivep)
    (sleep timeout)
    (kill-root))
  ;; tried two times, if it isn't terminated by then, manual
  ;; intervention is needed
  (sleep 0.1)
  (if (root-alivep)
      (warn "root process wasn't restarted")
      (spawn-thread #'node-thread
		    :pid root-pid
		    :name "DISTLISP-ROOT"
		    :traps-exit T)))
