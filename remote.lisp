;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun send-exit (pid reason &optional (from *current-pid*))
  (aif (pid-node pid)
       ;; if it's a remote process, send a message to the corresponding
       ;; root process
       (send-list (make-root-pid it) from :EXIT (pid-id pid) reason)
       (awhen (with-processes (find-process pid))
	 (with-slots (traps-exit) it
	   ;; (with-process-lock it
	   ;;   (remove-from-linked-set it from))
	   (when (or (eq reason :KILL) (eq reason T)
		     (and (not traps-exit)
			  (not (eq reason :NORMAL))
			  (not (eq reason NIL))))
	     (kill-process it reason))
	   (when (and (with-process-lock it traps-exit)
		      (not (or (eq reason :KILL) (eq reason T))))
	     (send-list pid from :EXIT reason))))))

(defun kill (pid &optional (reason :KILL))
  (send-exit pid reason))

(defun map-exit (from reason pids)
  (mapcar (lambda (pid) (send-exit pid reason from)) pids))

(defun %link (to &optional (process *current-process*)
	      &aux (pid (slot-value process 'pid)))
  (aif (pid-node to)
       (progn (send-list (make-root-pid it) pid :LINK (pid-id to))
	      (%receive-if ))
       (awhen (with-processes (find-process to))
	 (with-process-lock process
	   (with-process-lock it
	     (link-processes process pid it to))))))

(defun %unlink (to &optional (process *current-process*)
		&aux (pid (slot-value process 'pid)))
  (aif (pid-node to)
       (send-list (make-root-pid it) pid :UNLINK (pid-id to))
       (awhen (with-processes (find-process to))
	 (with-process-lock process
	   (with-process-lock it
	     (unlink-processes process pid it to))))))

(defun unlink (pid)
  (%unlink pid))

(defun link (pid)
  (%link pid))
