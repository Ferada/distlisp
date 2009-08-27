;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem :distlisp
  :depends-on (:alexandria
	       :cl-walker
	       :cl-syntax-sugar
	       :bordeaux-threads
	       :iolib
	       :logv
	       :utils-frahm
	       :fare-matcher
	       :anaphora
	       :cl-store
	       :trivial-timeout
	       :fiveam)
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
	       ))

(asdf:defsystem :distlisp-tests
  :depends-on (:distlisp :fiveam)
  :components ((:file "test")))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :distlisp))))
  (asdf:oos 'asdf:load-op :distlisp-tests)
  (fiveam:run! 'distlisp-tests::distlisp-suite))

(defmethod asdf:operation-done-p ((op asdf:test-op) (system (eql (asdf:find-system :distlisp))))
  nil)
