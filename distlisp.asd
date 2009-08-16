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
  :serial T
  :components ((:file "package")
	       (:file "defaults")
	       (:file "globals")
	       (:file "methods")
	       (:file "utils")
	       (:file "encode")
	       (:file "process")
	       (:file "receive")
	       (:file "node")
	       (:file "send")
	       (:file "remote")
	       (:file "spawn")
	       (:file "server")
	       (:file "environment")
	       (:file "services")
	       ;; (:file "test")
	       ))
