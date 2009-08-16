;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun send-exit (pid from &optional reason)
  (aif (pid-node pid)
       ;; if it's a remote process, send a message to the corresponding
       ;; root process
       (%send (make-root-pid it) `(:EXIT ,(pid-id it) ,reason) from)
       (awhen (with-processes (find-process pid))
	 (with-slots (traps-exit) it
	   (with-process-lock it
	     (remove-from-linked-set it from))
	   (when (or (eq reason :KILL) (eq reason T)
		     (and (not traps-exit)
			  (not (eq reason :NORMAL))
			  (not (eq reason NIL))))
	     (kill-process it reason))
	   (when (and traps-exit (not (or (eq reason :KILL) (eq reason T))))
	     (%send pid `(:EXIT ,reason) from))))))

(defun kill (pid &optional (reason :KILL))
  (send-exit pid *current-pid* reason))

(defun map-exit (from reason pids)
  (mapcar (lambda (pid) (send-exit pid from reason)) pids))

;; (defun link (pid)
;;   "Links foreign process death to current process."
;;   (with-processes (%link pid)))

;; (defun %link (pid &optional (process *current-process*))
;;   (awhen (find-process pid)
;;     (link-processes process (process-info-pid process) it pid)))
