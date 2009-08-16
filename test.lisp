(in-package #:cl-user)

(defpackage #:distlisp-tests
  (:use #:cl #:logv #:utils-frahm #:anaphora #:distlisp #:fare-matcher)
  (:export #:run-tests))

(in-package #:distlisp-tests)

;;;; two demos for process death, exits and linking from
;;;; Programming Erlang, p. 157

(defun wait (Prog)
  (receive-loop (Any From)
    (format T "process ~A received ~A from ~A~%" Prog Any From)))

(defun status (Name Pid)
  (if (distlisp::with-processes (distlisp::find-process Pid))
      (prog1 T (format T "process ~A (~A) is alive~%" Name Pid))
      (prog1 NIL (format T "process ~A (~A) is dead~%" Name Pid))))

(defun a ()
  (wait 'a))

(defun b (A)
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

(defun run-demo2 (Bool M)
  (let* ((A (spawn (:local :traps-exit T) (a)))
	 (B (spawn (:local :traps-exit Bool) (b A)))
	 (C (spawn () (c B M))))
    (sleep 0.1)
    (let ((sta (status 'a A))
	  (stb (status 'b B))
	  (stc (status 'c C)))
      (when stc (kill C))
      (when stb (kill B))
      (when sta (kill A))
      (list sta stb stc))))

(defun c2 (B M)
  (link B)
  (kill B M)
  (wait 'c))

(defun run-demo2 (Bool M)
  (let* ((A (spawn (:local :traps-exit T) (a)))
	 (B (spawn (:local :traps-exit Bool) (b A)))
	 (C (spawn (:local :traps-exit T) (c2 B M))))
    (sleep 0.1)
    (let ((sta (status 'a A))
	  (stb (status 'b B))
	  (stc (status 'c C)))
      (when stc (kill C))
      (when stb (kill B))
      (when sta (kill A))
      (list sta stb stc))))

(defun run-tests (calls fun)
  (dolist (call calls)
    (let* ((result (apply fun (car call)))
	   (successful (equalp result (cdr call))))
      (unless successful
	(format T "test ~A did not result in ~A but ~A, aborted~%"
		(car call) (cdr call) result)
	(return-from run-tests))))
  (format T "all tests passed~%")
  T)

(defun test-demo1 ()
  (let ((calls `(((T (die abc)) . (T T NIL))
		 ((T (die :NORMAL)) . (T T NIL))
		 ((T (die :KILL)) . (T T NIL))
		 ((T (divide 0)) . (T T NIL))
		 ((T NORMAL) . (T T NIL))
		 ((NIL (die abc)) . (T NIL NIL))
		 ((NIL (die :NORMAL)) . (T T NIL))
		 ((NIL (die :KILL)) . (T NIL NIL))
		 ((NIL (divide 0)) . (T NIL NIL))
		 ((NIL NORMAL) . (T T NIL)))))
    (run-tests calls #'run-demo1)))

(defun test-demo2 ()
  (let ((calls `(((NIL abc) . (T NIL T))
		 ((NIL :NORMAL) . (T T T))
		 ((NIL :KILL) . (T NIL T))
		 ((T abc) . (T T T))
		 ((T :NORMAL) . (T T T))
		 ((T :KILL) . (T NIL T)))))
    (run-tests calls #'run-demo2)))
