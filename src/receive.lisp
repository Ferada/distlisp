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

(defun recv-nowait (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Returns NIL if there's no message."
  (tuple-to-values (dequeue deque)))

(defun recv-if (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Blocks if none available."
  (tuple-to-values (dequeue-wait-if deque
				    (lambda (msg)
				      (funcall test (car msg) (cdr msg))))))

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
