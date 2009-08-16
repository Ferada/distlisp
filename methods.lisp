;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

;;;; method primitives

(in-package #:distlisp)

;;; see encode.lisp

(defgeneric encode (encoding stream message)
  (:documentation
   "Encodes a MESSAGE into a STREAM using the specified ENCODING."))

(defgeneric decode (encoding stream)
  (:documentation
   "Decodes a message from a STREAM using the specified ENCODING."))

;;; see process.lisp

(defgeneric kill-process (process &optional reason)
  (:documentation
   "Kills another PROCESS, running the usual cleanup forms."))

(defgeneric exit-process (process &optional reason)
  (:documentation
   "Exits the current PROCESS, running the usual cleanup forms."))


;;; see node.lisp

(defgeneric write-node (node message)
  (:documentation "Writes a MESSAGE out to NODE."))
