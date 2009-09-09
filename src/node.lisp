;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package :distlisp)

;;; data structures

(defclass node-info ()
  ((name :accessor node-info-name)
   encoding
   ;; processes in the monitor set will receive messages on error or disconnect
   monitored-set)
  (:documentation "Identifies a node and its encoding."))

(defclass socket-node-info (node-info)
  (socket
   ;; stream used to read from it (may be different from socket when
   ;; using encryption)
   stream
   ;; the reader thread
   thread)
  (:documentation "Specifies a node connected via a socket."))

;;; helper functions

(defun add-node (node)
  (push node *nodes*))

(defun remove-node (node)
  (setf *nodes* (delete node *nodes* :test #'eq)))

(defun find-node (name)
  "Searches *NODES* by NAME."
  (find name *nodes* :key #'node-info-name :test #'eq))

(defun valid-node? (node)
  "Returns a generalized boolean whether NODE is still valid to our knowledge."
  (member node *nodes* :test #'eq))

(defmethod write-node ((node socket-node-info) message)
  (with-slots (stream encoding) node
    (encode encoding stream message)))

;;; global API

;; (defclass global-node ()
;;   ()
;;   (:documentation "Describes the position of a node independent of the current one."))

;; (defclass inet-node (global-node)
;;   (host
;;    port))

;; (defmethod node->global (node))

;; (defmethod global->node (global))

;; (defmethod %connect ((global inet-node)) )
