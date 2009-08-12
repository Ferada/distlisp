;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:cl-user)

(defpackage #:distlisp
  (:use #:cl #:logv #:utils-frahm #:anaphora #:fare-matcher #:bordeaux-threads)
  (:export #:init-environment #:stop-environment
	   #:spawn
	   #:%send
	   #:%receive #:%receive-nowait))

(in-package #:distlisp)

;;;; utilities

;; (defun make-counter (limit &optional (lower 0) (initial (1- limit)))
;;   (when (>= lower limit)
;;     (error "LIMIT has to be bigger than LOWER (~D >= ~D" lower limit))
;;   (lambda () (if (= initial (1- limit)) (setf initial lower) (incf initial))))

;; (defun counter-next (counter)
;;   (funcall counter))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-counter (limit &optional (lower 0) (initial (1- limit)))
    (when (>= lower limit)
      (error "LIMIT has to be bigger than LOWER (~D >= ~D" lower limit))
    (vector initial limit lower)))

(defun counter-next (counter)
  (if (= (svref counter 0) (1- (svref counter 1)))
      (setf (svref counter 0) (svref counter 2))
      (incf (svref counter 0))))

(defconstant default-distlisp-bindings `((*package* . ,(lambda () #.(find-package :DISTLISP)))))

(let ((lock (make-lock))
      (sync (make-condition-variable)))
  (defun make-thread/synchronized (fun &key name (initial-bindings default-distlisp-bindings)
				   init)
    "Creates a new thread and returns if it's running."
    (with-lock-held (lock)
      (prog1 (make-thread (lambda ()
			    (with-lock-held (lock)
			      (unwind-protect
				   (when init
				     (funcall init))
				(condition-notify sync)))
			    (funcall fun))
			  :name name
			  :initial-bindings initial-bindings)
	(condition-wait sync lock)))))

;;;; basic API

;;; constants and parameters

(defparameter default-format '(:utf-8 :eol-style :lf)
  "Default format for all communications.")

(defparameter default-timeout 3
  "Default timeout for some operations (in seconds).")

(defconstant counter-size 16
  "The number of bits for counter wraparound (e.g. for pids).")

(defstruct mailbox
  (indeque (make-locked-deque))
  (outdeque (make-locked-deque)))

(defvar *processes* NIL
  "Global list of processes.")

(defvar *processes-lock* (make-lock)
  "Global lock for processes.")

(defmacro with-processes (&body body)
  "Executes BODY with *PROCESSES-LOCK* held."
  `(with-lock-held (*processes-lock*)
     ,@body))

(defvar* *current-process*
  "Thread-local process variable.")

(defvar* *current-pid*
  "Thread-local process ID.")

;;; helper functions

(defstruct process-info
  "Has information about a local process."
  pid
  (mailbox (make-mailbox))
  linked-set
  traps-exit
  )

(defun current-indeque ()
  (mailbox-indeque (process-info-mailbox *current-process*)))

(defstruct (thread-process-info (:include process-info))
  "Additional information for a thread based process."
  thread)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-pid-counter ()
    "Returns a counter useful for counting PIDs."
    (make-counter (ash 1 counter-size) 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((pid-counter (make-pid-counter)))
    (defstruct pid
      "Identifies a process relative to the local node."
      (id (counter-next pid-counter))
      node)))

(defun find-process (pid)
  "Searches *PROCESSES* by PID."
  (find pid *processes* :key #'process-info-pid :test #'equalp))

(defun valid-local-pid? (pid)
  "Returns a generalized boolean whether PID is still valid to our knowledge."
  (member pid *processes* :key #'process-info-pid :test #'equalp))

(let ((sync (make-condition-variable))
      (counter 0))
  (defun %spawn-thread (fun &key linked (pid (make-pid))
			(name (format NIL "DISTLISP-SPAWNED-~D" (incf counter))))
    "Spawns a new local process (using a thread).  Returns when the process is initialised."
    (let ((process-info (make-thread-process-info :pid pid)))
      (with-processes
	(make-thread (lambda ()
		       (with-processes
			 (setf (thread-process-info-thread process-info) (current-thread))
			 (when linked
			   (push pid (process-info-linked-set process-info)))
			 (push process-info *processes*)
			 (condition-notify sync))
		       (unwind-protect
			    (let ((*current-process* process-info)
				  (*current-pid* pid))
			      (funcall fun))
			 (with-processes
			   (setf *processes* (delete process-info *processes*))
			   (mapcar (lambda (pid)
				     (send-exit pid process-info))
				   (process-info-linked-set process-info))
			   )))
		     :name name
		     :initial-bindings default-distlisp-bindings)
	(condition-wait sync *processes-lock*))
      pid)))

(defun quit-process (process)
  (etypecase process
    (thread-process-info (destroy-thread (thread-process-info-thread process)))))

(defun exit ()
  "Exits the current thread, running the usual cleanup forms."
  (quit-process *current-process*))

(defun quit-node (node)
  ;; the node is removed inside the reader thread
  (destroy-thread (socket-node-info-thread node)))

;; (defun %interrupt-thread (pid &optional (fun (lambda () (error "interrupted"))))
;;   (interrupt-thread (thread-process-info-thread (find-process pid)) fun))

(defun %link (pid &optional (process *current-process*))
  (awhen (find-process pid)
    (etypecase it
      (thread-process-info (push (process-info-pid process) (process-info-linked-set it))))))

;;; method primitives

(defgeneric encode (encoding stream message)
  (:documentation "Encodes a MESSAGE into a STREAM using the specified ENCODING."))

(defgeneric decode (encoding stream)
  (:documentation "Decodes a message from a STREAM using the specified ENCODING."))

;;; receiving messages variants

(defun %receive (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Blocks if none available."
  (dequeue-wait deque))

(defun %receive-nowait (&optional (deque (current-indeque)))
  "Gets the next message from the mailbox.  Returns NIL if there's no message."
  (dequeue deque))

(defun %receive-match (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Blocks if none available."
  (dequeue-wait-match deque (lambda (message) (funcall test (car message) (cdr message)))))

(defun %receive-match-nowait (test &optional (deque (current-indeque)))
  "Gets the next matching message from the mailbox.  Returns NIL if there's no message."
  (dequeue-match deque (lambda (message) (funcall test (car message) (cdr message)))))

;;;; extended API

(defvar *nodes* NIL
  "All nodes connected to the server.")

(defvar *nodes-lock* (make-lock)
  "Protects the special variable *NODES*.")

(defmacro with-nodes (&body body)
  `(with-lock-held (*nodes-lock*)
     ,@body))

(defstruct node-info
  "Identifies a node and its encoding."
  name
  encoding
  linked-set)

(defstruct (socket-node-info (:include node-info))
  "Specifies a node connected via a socket."
  socket
  ;; stream used to read from it (may be different from socket when
  ;; using encryption)
  stream
  ;; the reader thread
  thread
  )

(defconstant root-pid (make-pid :id 0)
  "Specifies the local node process PID.")

(defun make-root-pid (node)
  (make-pid :id 0 :node node))

(defun add-node (node)
  (push node *nodes*))

(defun remove-node (node)
  (setf *nodes* (delete node *nodes* :test #'eq)))

(defun find-node (name)
  "Searches *NODES* by NAME."
  (find name *nodes* :key #'node-info-name :test #'eq))

(defun valid-node? (node)
  "Returns a generalized boolean whether NODE is still valid to our knowledge."
  (member node *nodes* :test #'eq))

(defun %write-node (node message encoding)
  (etypecase node
    (socket-node-info (encode encoding (socket-node-info-stream node) message))))

(defun %wrap-local (message from)
  "<MESSAGE> . FROM"
  (cons message from))

(defun route-remote (to from message)
  (check-type to pid)
  (check-type from pid)
  (let* ((node (pid-node to))
	 (encoding (node-info-encoding node)))
    (if (with-nodes (valid-node? node))
	(progn (%write-node node (%wrap-remote to message from) encoding)
	       T)
	(warn "remote process ~A on node ~A not valid, discarded"
	      (pid-id to) node))))

(defun route-local (to from message)
  (check-type to pid)
  (check-type from pid)
  (aif (with-processes (find-process to))
       (progn (enqueue (mailbox-indeque (process-info-mailbox it))
		       (%wrap-local message from))
	      T)
       (warn "local process ~A not found, discarded" (pid-id to))))

(defun %send (to message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to another process."
  (check-type to pid)
  (if (pid-node to)
      (route-remote to from message)
      (route-local to from message)))

(defun %send-node (node message &optional (from *current-pid*))
  "Queues a message for asynchronously sending it to the handler process at NODE."
  (check-type node node-info)
  (route-remote (make-root-pid node) from message))

(defun %wrap-remote (to message from)
  "PIDTO PIDFROM <MESSAGE>"
  `(,(pid-id to) ,(pid-id from) ,message))

(defun reader-thread (node encoding stream &aux error)
  "Reads data from a STREAM using ENCODING."
  (unwind-protect
       (handler-case
	   (loop (destructuring-bind (pidto pidfrom message) (decode encoding stream)
		   (let ((to (make-pid :id pidto))
			 (from (make-pid :id pidfrom :node node)))
		     (unless (route-local to from message)
		       ;; send error message back
		       (route-remote from root-pid `(:NOPROCESS ,pidto))))))
	 ;; if we get an error, save it, so linked process can examine it
	 (error (e)
	   (setf error e)
	   (error e)))
    (with-nodes 
      (remove-node node)
      (close stream))
    (let ((remote-root (make-root-pid node)))
      (dolist (pid (node-info-linked-set node))
	(route-local pid remote-root (if error
					 `(:ERROR ,error)
					 :DISCONNECTED))))))

(defun make-connect-thread (socket name encoding &optional (stream socket))
  (let ((node (make-socket-node-info :encoding encoding :socket socket :stream stream)))
    (make-thread/synchronized (lambda ()
				(setf (iolib:fd-non-blocking socket) T)
				(reader-thread node encoding stream))
			      :name name
			      :init (lambda ()
				      (setf (socket-node-info-thread node)
					    (current-thread))))
    node))

(defun make-client-socket (host port)
  (iolib:make-socket :ipv6 NIL
		     :reuse-address T
		     :remote-host host
		     :remote-port port))

(defun %connect (host port encoding)
  "Connects this node to another node and returns a handle if successful."
  (make-connect-thread (make-client-socket host port)
		       (format NIL "DISTLISP-READER-~A-~D" host port)
		       encoding))

(defun node-acceptor (server fd event exception encoding)
  "Handles a connection attempt and starts the reader thread for it."
  (let* ((socket (iolib.sockets:accept-connection server))
	 (node (make-connect-thread socket
				    (format NIL "DISTLISP-READER-~A-~D"
					    (iolib.sockets:remote-host socket)
					    (iolib.sockets:remote-port socket))
				    encoding)))
    (with-nodes
      (add-node node))))

(defun add-server-handler (base server encoding)
  (iolib.base:unwind-protect-case ()
      (progn
	(setf (iolib:fd-non-blocking server) T)
	(iomux:set-io-handler base (iolib:socket-os-fd server) :read
			      (lambda (fd event exception)
				(node-acceptor server fd event exception encoding))))
    (:abort (close server))))

(defun make-server-socket (port)
  (iolib:make-socket :connect :passive
		     :ipv6 NIL
		     :reuse-address T
		     :local-host iolib:+ipv4-unspecified+
		     :local-port port
		     :external-format default-format))

;; TODO: pre-protocol for establishing a connection, like, configuring the encoding
(defun accept-thread (port encoding &aux done)
  (let ((base (make-instance 'iomux:event-base)))
    (unwind-protect
	 (let ((plain-socket (make-server-socket port)))
	   (add-server-handler base plain-socket encoding)
	   (with-open-stream (socket plain-socket)
	     (loop until done
		do (iomux:event-dispatch base))))
      (close base))))

;; TODO: register started server threads somewhere
(defun start-server (&optional (port 2000) (encoding :sexp))
  (make-thread/synchronized (lambda () (accept-thread port encoding))
			    :name (format NIL "DISTLISP-ACCEPT-~D" port)))

(defun connect (name host &optional (port 2000) (encoding :sexp))
  "Connects this node to another node on PORT under NAME."
  (let ((node (%connect host port encoding)))
    (setf (node-info-name node) name)
    (with-nodes
      (add-node node))
    node))

;;; en- and decoding methods

(defmethod encode ((encoding (eql :sexp)) stream message)
  (write message :stream stream)
  (terpri stream)
  (finish-output stream))

(defmethod decode ((encoding (eql :sexp)) stream)
  (read stream))

(defmethod encode ((encoding (eql :clstore)) stream message)
  (cl-store:store message stream)
  (finish-output stream))

(defmethod decode ((encoding (eql :clstore)) stream)
  (cl-store:restore stream))

;;;; initialisation

(defun register-current-thread ()
  (unless (member (current-thread) *processes*
		  :key (lambda (process) (when (thread-process-info-p process)
					   (thread-process-info-thread process)))
		  :test #'eq)
    (let* ((pid (make-pid))
	   (info (make-thread-process-info :pid pid
					   :thread (current-thread))))
      (setf *current-process* info
	    *current-pid* pid)
      (push info *processes*))))

(defun quit-root ()
  (route-local root-pid *current-pid* :QUIT))

(defun stop-environment (&optional (timeout 3))
  (flet ((kill-nodes ()
	   (with-nodes (dolist (node *nodes*)
			 (quit-node node))))
	 (kill-processes ()
	   (with-processes (dolist (process *processes*)
			     (quit-process process)))))
    (quit-root)
    (kill-nodes)
    (kill-processes)
    (when (or #1=(with-processes *processes*) #2=(with-nodes *nodes*))
      (sleep timeout)
      (kill-nodes)
      (kill-processes))
    (when #1#
      (warn "some processes still alive"))
    (when #2#
      (warn "some nodes still connected"))))

;;;; macros and convenience functions based on the previous APIs

(let ((counter (make-pid-counter)))
  (defun %spawn-remote (node fun &optional linked)
    (let ((num (counter-next counter)))
      (%send (make-root-pid node) `(:SPAWN ,num ,linked ,fun))
      (cdr (%receive-match (lambda (message from)
			     (and (eq (car message) :SPAWNED)
				  (= (cadr message) num))))))))

(defmacro! spawn ((target &optional linked) &body fun)
  "Spawns lambda form FUN at TARGET.

TARGET can be of type (OR NULL KEYWORD NODE-INFO).

If TARGET is a keyword, :LOCAL specifies thread spawning."
  (cond
    ;; this should become a load balancer or something strategy dependent (?!)
    ;; ((null target)
    ;;  `(%spawn-thread (lambda () ,@fun) :linked ,linked))
    ((keywordp target)
     (ematch target
       (:local `(%spawn-thread (lambda () ,@fun) :linked ,linked))))
    (T 
     `(let ((,g!target ,target))
	(if (node-info-p ,g!target)
	    (%spawn-remote ,g!target '(lambda () ,@fun))
	    (error "unknown target specifier ~A" ,g!target))))))

;; TODO: needs WHO was killed!, see :killed message
(defun send-exit (pid from)
  (aif (pid-node pid)
       ;; if it's a remote process, send a message to the corresponding
       ;; root process
       (%send (make-root-pid it) `(:EXIT ,(pid-id it)) from)
       (awhen (find-process pid)
	 (if (process-info-traps-exit it)
	     (%send pid :EXIT from)
	     (quit-process it)))))

(defun link (pid)
  "Links foreign process death to current process."
  (with-processes (%link pid)))

(defmacro send (pid message-constructor))



(defun node-thread (&aux done)
  (handler-case
      (loop until done
	 do (destructuring-bind (message . from) (%receive)
	      (cond 
		((atom (logv message))
		 (case message
		   ;; only local processes may terminate this thread
		   (:QUIT (unless (pid-node from)
			    (setf done T)))
		   ;; only remote nodes can be disconnected
		   (:DISCONNECT (awhen (pid-node from)
				  (quit-node it)))))
		((listp message)
		 (case (car message)
		   (:EXIT (destructuring-bind (op id) message
			    (send-exit (make-pid :id id) from)))
		   (:SPAWN (destructuring-bind (op num fun) message
			     (spawn (:local)
			       (%send from `(:SPAWNED ,num))
			       (funcall (eval fun))))))))))
    (error (e)
      (logv e)
      (setf done T))))

(defun init-environment (&optional (timeout 3))
  (flet ((root-alivep ()
	   (with-processes (find-process #1=root-pid))))
    (with-processes (register-current-thread))
    (when (root-alivep)
      (quit-root))
    (sleep 0.1)
    (when (root-alivep)
      (sleep timeout)
      (quit-root))
    ;; tried two times, if it isn't terminated by then, manual
    ;; intervention is needed
    (sleep 0.1)
    (if (root-alivep)
	(warn "root process wasn't restarted")
	(%spawn-thread #'node-thread :pid #1# :name "DISTLISP-ROOT"))))
