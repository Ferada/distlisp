;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:distlisp
  (:use #:cl #:logv #:utils-frahm #:anaphora #:fare-matcher #:bordeaux-threads)
  (:export #:init-environment #:stop-environment
	   #:spawn
	   #:%send
	   #:%receive #:%receive-nowait #:%receive-match #:%receive-match-nowait
	   #:with-registered-service
	   #:register-current-thread
	   #:make-root-pid #:pid-eq
	   #:receive-bind #:receive-loop
	   #:link #:exit #:kill))
