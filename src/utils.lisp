;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defun make-counter (limit &optional (lower 0) (initial (1- limit)))
  (when (>= lower limit)
    (error "LIMIT has to be bigger than LOWER (~D >= ~D" lower limit))
  (vector initial limit lower))

(defun counter-next (counter)
  (if (= (svref counter 0) (1- (svref counter 1)))
      (setf (svref counter 0) (svref counter 2))
      (incf (svref counter 0))))

(defun make-pid-counter ()
  "Returns a counter useful for counting PIDs."
  (make-counter (ash 1 counter-size) 1))

(let ((lock (make-lock "make-thread/synchronised-lock"))
      (sync (make-condition-variable)))
  (defun make-thread/synchronised (fun &key name init
				   (initial-bindings default-distlisp-bindings))
    "Creates a new thread and returns if it's running."
    (let (notified)
      (with-lock-held (lock)
	(let ((thread (make-thread (lambda ()
				     (with-lock-held (lock)
				       (unwind-protect
					    (when init
					      (funcall init))
					 (setf notified T)
					 (condition-notify sync)))
				     (funcall fun))
				   :name name
				   :initial-bindings initial-bindings)))
	  (loop (condition-wait sync lock)
	     (when notified
	       (return thread))
	     (unless (thread-alive-p thread)
	       (return))))))))
