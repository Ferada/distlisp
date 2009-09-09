;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; package: distlisp; -*-

(in-package #:distlisp)

(defconstant root-pid (make-pid :id 0)
  "Specifies the local node process PID.")

(defun kill-node (node)
  ;; the node is removed inside the reader thread
  (destroy-thread (slot-value node 'thread)))

(defun root-alivep ()
  (with-processes (find-process root-pid)))

(defun kill-root ()
  "Stops the node process."
  (route-local root-pid *current-pid* :QUIT))

(defun node-thread ()
  (receive-loop (message from)
    (if (atom message)
	(case message
	  ;; only local processes may terminate this thread
	  (:QUIT (unless (pid-node from)
		   (return)))
	  ;; only remote nodes can be disconnected
	  (:DISCONNECT (awhen (pid-node from)
			 (kill-node it)))
	  (:LIST-SERVICES (with-services
			    (apply #'send-list from *current-pid* :SERVICES *services*)))
	  (T (logv 'unknown-message message)))
	(case (car message)
	  (:EXIT (destructuring-bind (op id reason) message
		   (declare (ignore op))
		   (send-exit (make-pid :id id) reason from)))
	  ;; TODO: implement this (in a generic way, if possible)
	  (:LINK)
	  (:UNLINK)
	  (:TRAP-EXIT)
	  (:SPAWN (destructuring-bind (op linked monitored traps-exit fun)
		      message
		    (declare (ignore op))
		    (spawn (:local :traps-exit traps-exit)
		      (with-process-lock *current-process*
			(when linked
			  (add-to-linked-set *current-process* from))
			(when (or (eq monitored :FROM)
				  (eq monitored :BOTH))
			  (add-to-monitored-set *current-process* from)))
		      (%send from :SPAWNED)
		      (funcall (eval fun)))))
	  (T (logv 'unknown-message message))))))

(defun reader-thread (node encoding stream &aux error)
  "Reads data from a STREAM using ENCODING."
  (unwind-protect
       (handler-case
	   (loop (destructuring-bind (pidto pidfrom message)
		     (decode encoding stream)
		   (let ((to (make-pid :id pidto))
			 (from (make-pid :id pidfrom :node node)))
		     (unless (with-processes (route-local to from message))
		       ;; send error message back
		       (with-nodes
			 (route-remote from root-pid `(:NOPROCESS ,pidto)))))))
	 ;; if we get an error, save it, so linked process can examine it
	 (error (e)
	   (setf error e)))
    (with-nodes 
      (remove-node node)
      (close stream))
    (map-exit (with-nodes (copy-list (slot-value node 'monitored-set)))
	      (make-root-pid node)
	      (if error error :DISCONNECTED))))

(defun make-connect-thread (socket name encoding &optional (stream socket))
  (let ((node (make-instance 'socket-node-info :encoding encoding :socket socket :stream stream)))
    (make-thread/synchronised
     (lambda ()
       (setf (iolib:fd-non-blocking socket) T)
       (reader-thread node encoding stream))
     :name name
     :init (lambda ()
	     (with-nodes
	       (setf (slot-value node 'thread)
		     (current-thread)))))
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
		     :external-format default-external-format))

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
  (make-thread/synchronised (lambda () (accept-thread port encoding))
			    :name (format NIL "DISTLISP-ACCEPT-~D" port)))

(defun connect (name host &key (port 2000) (encoding :sexp) monitored)
  "Connects this node to another node on PORT under NAME.  If LINKED is
set, the connecting process gets added to the list of linked processes
for the node."
  (let ((node (%connect host port encoding)))
    (setf (node-info-name node) name)
    (when monitored
      (push *current-pid* (slot-value node 'monitored-set)))
    (with-nodes
      (add-node node))
    node))
