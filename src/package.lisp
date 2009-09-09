;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: cl-user; -*-

(in-package :cl-user)

(defpackage distlisp
  (:use :cl :logv :utils-frahm :anaphora :fare-matcher :bordeaux-threads)
  (:export :init-environment :stop-environment
	   :spawn
	   :%send
	   :recv :recv-nowait :recv-if :recv-if-nowait
	   :with-registered-service
	   :register-current-thread
	   :make-root-pid :pid-eq
	   :receive-bind :receive-loop :receive
	   :link :unlink
	   :exit :kill))
