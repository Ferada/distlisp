;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package :distlisp)

(defun register-service (name &optional (pid *current-pid*))
  (with-services
    (push (cons name pid) *services*)))

(defun unregister-service (&optional (pid *current-pid*))
  (with-services
    (setf *services* (delete pid *services* :key #'cdr :test #'pid-eq))))

(defmacro with-registered-service (name &body body)
  `(unwind-protect
	(progn
	  (register-service ',name)
	  ,@body)
     (unregister-service)))

(defun list-services (node)
  (let ((pid (make-root-pid node)))
    (send pid :LIST-SERVICES)
    (car (recv-if (lambda (message from)
		    (and (eq (car message) :SERVICES)
			 (pid-eq from pid)))))))
