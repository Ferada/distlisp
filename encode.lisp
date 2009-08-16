;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

;;; simple lisp reader based serialisation

(defmethod encode ((encoding (eql :sexp)) stream message)
  (with-standard-io-syntax
    (let ((*print-circle* T))
      (write message :stream stream)
      (terpri stream)
      (finish-output stream))))

(defmethod decode ((encoding (eql :sexp)) stream)
  (with-standard-io-syntax
    (let ((*read-eval* NIL))
      (read stream))))

;;; serialisation based on cl-store

;; TODO: use #+cl-store and asdf features for conditional compilation

(defmethod encode ((encoding (eql :clstore)) stream message)
  (cl-store:store message stream)
  (finish-output stream))

(defmethod decode ((encoding (eql :clstore)) stream)
  (cl-store:restore stream))
