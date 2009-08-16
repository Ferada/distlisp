;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defstruct mailbox
  (indeque (make-locked-deque))
  (outdeque (make-locked-deque)))

(defstruct pid
  "Identifies a process relative to the local node."
  id 
  node)

(defconstant pid-counter (make-pid-counter))

(defclass process-info ()
  ((pid :reader process-info-pid)
   (mailbox :initform (make-mailbox) :reader process-info-mailbox)
   linked-set
   monitored-set
   traps-exit
   (lock :initform (make-lock "process-lock")))
  (:documentation "Has information about a local process."))

(defmacro! with-process-lock (process &body body)
  `(with-slots ((,g!lock lock)) ,process
     (with-lock-held (,g!lock)
       ,.body)))

(defclass thread-process-info (process-info)
  (thread)
  (:documentation "Additional information for a process based on a thread."))

;;; helper functions

(defun make-root-pid (node)
  (make-pid :id 0 :node node))

(defun pid-eq (p q)
  "Returns T if P and Q specify the same process, else NIL."
  (and (= (pid-id p) (pid-id q))
       (eq (pid-node p) (pid-node q))))

(defun add-process (process)
  (push process *processes*))

(defun remove-process (process)
  (setf *processes* (delete process *processes* :test #'eq)))

(defun find-process (pid)
  "Searches *PROCESSES* by PID."
  (find pid *processes* :key #'process-info-pid :test #'pid-eq))

(defun pid-counter-next (counter)
  (loop as result = (counter-next counter)
     while (find-process result)
     finally (return result)))

;;; helper functions for process-info

(defun add-to-linked-set (process pid)
  (with-slots ((set linked-set)) process
    (pushnew pid set :test #'pid-eq)))

(defun remove-from-linked-set (process pid)
  (with-slots ((set linked-set)) process
    (setf set (delete pid set :test #'pid-eq))))

(defun link-processes (process-1 pid-1 process-2 pid-2)
  (add-to-linked-set process-1 pid-2)
  (add-to-linked-set process-2 pid-1))

(defun exit (&optional (reason :NORMAL))
  "Exits the current process, running the usual cleanup forms."
  (exit-process *current-process* reason))

;;; specific implementation of thread based process

(defmethod kill-process ((process thread-process-info) &optional (reason :KILL))
  (interrupt-thread (slot-value process 'thread)
		    (lambda () (exit reason))))

(define-condition abort-thread-condition (condition)
  ((reason :accessor reason :initarg :reason)))

(defmethod exit-process ((process thread-process-info) &optional (reason :KILL))
  (unless (signal 'abort-thread-condition :reason reason)
    #+sbcl
    (sb-ext:quit)
    ;; not legal to kill calling thread, says the documentation
    ;; (destroy-thread (thread-process-info-thread *current-process*))
    ))
