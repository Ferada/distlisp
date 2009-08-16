(in-package #:cl-user)

(defpackage #:distlisp-tests
  (:use #:cl #:logv #:utils-frahm #:anaphora #:distlisp #:fare-matcher)
  (:export #:run-tests))

(in-package #:distlisp-tests)

(defun wait (Prog)
  (receive-loop (Any From)
    (format T "Process ~A received ~A from ~A~%" Prog Any From)))

(defun status (Name Pid)
  (if (distlisp::with-processes (distlisp::find-process Pid))
      (format T "process ~A (~A) is alive~%" Name Pid)
      (format T "process ~A (~A) is dead~%" Name Pid)))

(defun a ()
  (distlisp::with-processes (setf (distlisp::process-info-traps-exit distlisp::*current-process*) T))
  (wait 'a))

(defun b (A Bool)
  (distlisp::with-processes (setf (distlisp::process-info-traps-exit distlisp::*current-process*) Bool))
  (link A)
  (wait 'b))

(defun c (B M)
  (link B)
  (ematch M
	  (`(die ,reason)
	    (exit reason))
	  (`(divide ,N)
	    (if (= N 0)
		(error 'division-by-zero)
		(/ 1 N))
	    (wait 'c))
	  (normal T)))

(defun start (Bool M)
  (let* ((A (spawn () (a)))
	 (B (spawn () (b A Bool)))
	 (C (spawn () (c B M))))
    (sleep 1)
    (status 'b B)
    (status 'c C)
    ;;(logv A B C)
    (sleep 3)
    (kill C)
    (kill B)
    (kill A)))



(defun run-tests ())
