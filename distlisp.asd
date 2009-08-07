;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem :distlisp
  :depends-on (#:alexandria
	       #:cl-walker
	       #:cl-syntax-sugar
	       #:bordeaux-threads
	       #:iolib
	       #:logv
	       #:utils-frahm
	       #:fare-matcher
	       #:anaphora
	       #:cl-store
	       #:trivial-timeout)
  :components ((:file "distlisp")))
