;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(let ((sync (make-condition-variable)))
  (defun spawn-thread (fun &key linked monitored traps-exit
		       (pid (make-pid :id (with-processes (pid-counter-next pid-counter))))
		       (name (format NIL "DISTLISP-SPAWNED-~D" (pid-id pid))))
    "Spawns a new local process (using a thread).  Returns when the thread is running."
    (let ((process-info (make-instance 'thread-process-info :pid pid :traps-exit traps-exit))
	  (exit :NORMAL)
	  notified)
      (with-slots (thread linked-set monitored-set) process-info
	(let ((thread-fun (lambda ()
			    (unwind-protect
				 (handler-case
				     (progn
				       (with-process-lock process-info
					 (setf thread (current-thread))
					 (when linked
					   ;; links work both ways
					   ;; TODO: what happens, if one of the process is killed
					   ;; in the meantime?
					   (link-processes process-info
							   pid
							   *current-process*
							   *current-pid*))
					 ;; TODO: do this right
					 (when (or (eq monitored :FROM)
						   (eq monitored :BOTH))
					   (add-to-monitored-set process-info pid)))
				       (with-process-lock *current-process*
					 (when (or (eq monitored :TO)
						   (eq monitored :BOTH))
					   (add-to-monitored-set *current-process* pid)))
				       (with-processes
					 (add-process process-info)
					 (setf notified T)
					 (condition-notify sync))
				       (let ((*current-process* process-info)
					     (*current-pid* pid))
					 (funcall fun)))
				   (error (err)
				     (setf exit err))
				   (abort-thread-condition (abort)
				     (setf exit (reason abort))))
			      (with-processes
				;; be sure the callee is notified
				(unless notified
				  (condition-notify sync))
				(remove-process process-info))
			      (mapcar (lambda (to)
					(send-list to pid :KILLED exit))
				      (with-process-lock process-info
					(copy-list monitored-set)))
			      (map-exit pid
					(if (or (eq exit :KILL) (eq exit T))
					    :KILLED
					    exit)
					(with-process-lock process-info
					  (copy-list linked-set)))))))
	  ;; synchronise thread construction, the function may only return if
	  ;; the thread is started successfully
	  (with-processes
	    (let ((thread (make-thread
			   thread-fun
			   :name name
			   :initial-bindings default-distlisp-bindings)))
	      ;; return if either the process is in the process list, or it has
	      ;; died in the meantime
	      (loop (condition-wait sync *processes-lock*)
		 (when notified
		   (return pid))
		 (unless (thread-alive-p thread)
		   (return))))))))))

(defun spawn-remote (node fun &key linked monitored traps-exit)
  "Spawns FUN on NODE.  Returns if we receive a :SPAWNED message from the
spawned process."
  (let ((pid (make-root-pid node)))
    (send-list pid *current-pid* :SPAWN linked monitored traps-exit fun)
    (let ((remote (cdr (%receive-if (lambda (message from)
				      (and (eq (car message) :SPAWNED)
					   (pid-eq pid from)))))))
      (with-process-lock *current-process*
	(when linked
	  (add-to-linked-set *current-process* remote))
	(when (or (eq monitored :TO)
		  (eq monitored :BOTH))
	  (add-to-monitored-set *current-process* pid)))
      remote)))

;;; derived macros

(defmacro! spawn ((&optional (target :local) &key linked monitored traps-exit) &body fun)
  "Spawns lambda form FUN at TARGET.

TARGET can be of type (OR KEYWORD NODE-INFO).

If TARGET is a keyword, :LOCAL specifies thread spawning."
  ;;(check-type linked boolean)
  ;;(check-type traps-exit boolean)
  ;;(check-type monitored (member :BOTH :FROM :TO NIL))
  (cond
    ;; this should become a load balancer or something strategy dependent (?!)
    ;; ((null target)
    ;;  `(%spawn-thread (lambda () ,@fun) :linked ,linked))
    ((keywordp target)
     (ematch target
	     (:local `(spawn-thread (lambda () ,@fun)
				    :linked ,linked
				    :monitored ,monitored
				    :traps-exit ,traps-exit))))
    (T 
     `(let ((,g!target ,target))
	(if (node-info-p ,g!target)
	    (spawn-remote ,g!target '(lambda () ,@fun)
			  :linked ,linked
			  :monitored ,monitored
			  :traps-exit ,traps-exit)
	    (error "unknown target specifier ~A" ,g!target))))))
