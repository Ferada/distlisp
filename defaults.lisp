;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defparameter default-format '(:utf-8 :eol-style :lf)
  "Default format for all communications.")

(defparameter default-timeout 3
  "Default timeout for some operations (in seconds).")

(defconstant counter-size 16
  "The number of bits for counter wraparound (e.g. for pids).")

(defconstant default-distlisp-bindings
  `((*package* . ,(lambda () #.(find-package :DISTLISP))))
  "When a thread is started, this defaults are used for its initial
variable bindings.  See BORDEAUX-THREADS:MAKE-THREAD for more information.")
