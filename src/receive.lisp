;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package :distlisp)

(defun current-indeque ()
  (mailbox-indeque (slot-value *current-process* 'mailbox)))

;;; receiving messages variants
(defun tuple-to-values (cons)
  (when cons
    (values (car cons) (cdr cons))))

(defun recv (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Blocks if none available."
  (tuple-to-values (dequeue-wait deque)))

(defun recv-timeout (timeout &optional (deque (current-indeque)))
  (tuple-to-values (dequeue-wait-timeout deque timeout)))

(defun recv-nowait (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Returns NIL if there's no message."
  (tuple-to-values (dequeue deque)))

(defun recv-if (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Blocks if none available."
  (tuple-to-values (dequeue-wait-if deque
				    (lambda (msg)
				      (funcall test (car msg) (cdr msg))))))

(defun recv-if-timeout (test timeout &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Blocks if none available."
  (tuple-to-values (dequeue-wait-if-timeout deque
					    (lambda (msg)
					      (funcall test (car msg) (cdr msg)))
					    timeout)))

(defun recv-if-nowait (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Returns NIL if there's
no message."
  (tuple-to-values (dequeue-if deque (lambda (msg)
				       (funcall test (car msg) (cdr msg))))))

;;; derived macros

(defmacro! receive-bind ((&optional (msg NIL msgp) (from NIL fromp)) &body body)
  "Receives a message, binding it to the variable in MSG, default MSG and
the sender to the symbol in FROM, default FROM."
  (unless msgp
    (setf msg (intern (string :msg))))
  (unless fromp
    (setf from (intern (string :from))))
  `(multiple-value-bind (,msg ,from) (recv)
     ,.body))

(defmacro! receive-loop ((&optional (msg NIL msgp) (from NIL fromp)) &body body)
  (unless msgp
    (setf msg (intern (string :msg))))
  (unless fromp
    (setf from (intern (string :from))))
  `(loop with ,msg and ,from
      do (progn (multiple-value-setq (,msg ,from) (recv))
		,.body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-extended-lambda-arguments (args)
    "Parses an extended lambda argument list into a uniform association list.

(:FOO 1 :BAR 2 (:BAZ 32 54)) parses to ((:FOO . 1) (:BAR . 2) (:BAZ 32 54))."
    (loop while args
       as arg = (pop args)
       if (atom arg)
       if (null args)
       do (error "missing value for keyword ~A" arg)
       else collect (cons arg (pop args))
       else collect arg)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-wrap-after (block after form)
    (unless after
      (return-from maybe-wrap-after form))
    (destructuring-bind (timeout &body body) after
      (declare (ignore timeout))
      `(handler-case ,form
	 (trivial-timeout:timeout-error ()
	   (return-from ,block (progn ,.body)))))))

;;; at the moment, this is horribly inefficient (although, that goes for
;;; the matcher even more so)

;;; if we can hook into the matcher, we could get the variables in question
;;; so we don't have to create closures

(defmacro! receive (options &rest cases)
  "Receives messages matching at least one case.

Combines several functions using the supplied OPTIONS, which can be a
combination of the following parameters:

:NOWAIT BOOLEAN, uses non-blocking receive operation,
:AFTER NUMBER FORM*, runs the FORM* after a timeout of NUMBER seconds,
:FROM SYMBOL, binds the sender PID to the supplied symbol.

Could be used as follows:

(RECEIVE (:NOWAIT T (:AFTER 10 (error \"timeout\")))
  (x x))"
  (setf options (parse-extended-lambda-arguments options))
  (flet ((option (option)
	   (awhen (assoc option options)
	     (cdr it))))
    (let ((from (option :from))
	  (nowait (option :nowait))
	  (after (option :after)))
      (when (and nowait after)
	(warn "ignoring option AFTER because NOWAIT is set"))
      `(block ,g!block
	 (let (,g!match ,.(awhen from
			    (list from)))
	   ,(maybe-wrap-after
	     g!block
	     after
	     `(,(if nowait
		    'recv-if-nowait
		    (if after
			'recv-if-timeout
			'recv-if))
		(lambda (,g!msg ,g!from)
		  (declare (ignorable ,g!from))
		  ,.(when from
		      `((setf ,from ,g!from)))
		  (match ,g!msg
			 ,.(mapcar (lambda (case)
				     `(,(car case)
					(setf ,g!match (lambda () ,.(cdr case))) T))
				   cases)))
		;; this inserts the number of seconds for the timeout, if
		;; necessary
		,.(when (and (not nowait) after)
		    (list (car after)))))
	   (when ,g!match
	     (funcall ,g!match)))))))
