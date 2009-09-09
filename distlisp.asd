;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package :cl-user)

(defpackage distlisp-system
  (:use :cl :asdf))

(in-package :distlisp-system)

(defsystem :distlisp
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
  :components ((:module src
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
			     (:file "services")))))

(defsystem :distlisp-tests
  :depends-on (:distlisp :fiveam)
  :components ((:module src
		:serial T
		:components ((:file "tests")))))

(defmethod perform ((op test-op) (system (eql (find-system :distlisp))))
  (operate 'load-op :distlisp-tests)
  (operate 'test-op :distlisp-tests :force T))

(defmethod perform ((op test-op) (system (eql (find-system :distlisp-tests))))
  (or (every (symbol-function (intern (string :test-passed-p) (string :fiveam)))
	     (funcall (intern (string :run!) (string :fiveam))
		      (intern (string :distlisp-suite) (string :distlisp-tests))))
      (error "test-op failed")))
