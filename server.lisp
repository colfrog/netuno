(in-package :netuno)

(defvar *address* "127.0.0.1")
(defvar *port* 11111)
(defvar *listen-thread* nil)
(defvar *socket* nil)
(defvar *connections* (make-hash-table :test 'equal))
(defvar *player-conns* (make-hash-table :test 'equal))

(defun show-players-conn (conn)
  (format (socket-stream conn) "List of players: ~{~A~^, ~}~%" *players*)
  (force-output (socket-stream conn)))

(defun show-players (name)
  (let ((conn (gethash name *player-conns*)))
    (show-players-conn conn)))

(defun show-player-hand (name)
  (let ((conn (gethash name *player-conns*)))
    (when conn
      (format (socket-stream conn) "Your hand: ~a~%"
	      (hand-to-string (get-hand name)))
      (force-output (socket-stream conn)))))

(defun show-top-card (&optional (name nil))
  (flet ((show-top-card-conn (c)
	   (format (socket-stream c) "Top card is: ~a~%"
		   (card-to-string *top-card*))
	   (force-output (socket-stream c))))

    (if name
	(let ((conn (gethash name *player-conns*)))
          (show-top-card-conn conn))
	(maphash (lambda (c n)
		   (declare (ignore n))
		   (show-top-card-conn c))
		 *connections*))))

(defun show-current-turn (&optional (name nil))
  (flet ((show-current-turn-conn (c n)
	   (if (equal n (current-player))
	       (format (socket-stream c) "It's your turn~%")
	       (format (socket-stream c) "It's ~a's turn~%" (current-player)))
	   (force-output (socket-stream c))))
    (if name
	(let ((conn (gethash name *player-conns*)))
	  (show-current-turn-conn conn name))
	(maphash (lambda (c n)
		   (show-current-turn-conn c n))
		 *connections*))))

(defun announce-turn ()
  (let ((name (current-player)))
    (show-current-turn)
    (show-top-card)
    (show-player-hand name)))

(defun get-connection-line (conn)
  (handler-case
      (string-trim '(#\newline #\return #\space)
		   (read-line (socket-stream conn) nil))
    (error (err)
      (declare (ignore err))
      "")))

(defun init-connection (conn)
  (show-players-conn conn)
  (format (socket-stream conn) "Please choose a nickname:~%")
  (force-output (socket-stream conn))
  (let* ((name (get-connection-line conn)))
    (when name
      (if (not (null (gethash name *player-conns*)))
	  (progn
	    (format (socket-stream conn) "Nickname already taken.~%")
	    (force-output (socket-stream conn))
	    (setf name (init-connection conn)))
	  (progn
	    (setf (gethash conn *connections*) name)
	    (setf (gethash name *player-conns*) conn)
	    (maphash (lambda (c n)
		       (declare (ignore n))
		       (format (socket-stream c) "~a has joined the game~%" name)
		       (force-output (socket-stream c)))
		     *connections*)
	    (force-output (socket-stream conn))
	    (add-player name)
	    (show-player-hand name)
	    (show-current-turn name)
	    (show-top-card name)
	    (force-output (socket-stream conn)))))
    name))

(defun deinit-connection (conn name)
  (when name
    (remove-player name)
    (remhash conn *connections*)
    (remhash name *player-conns*)
    (maphash (lambda (c n)
	       (declare (ignore n))
	       (format (socket-stream c) "~a has left~%" name)
	       (force-output (socket-stream c)))
	     *connections*))
  (socket-close conn))

(defun handle-connection (conn)
  (let ((name (init-connection conn)))
    (do ((line (get-connection-line conn)
	       (get-connection-line conn)))
	((or (null line) (and (>= (length line) 4) (equal (subseq line 0 4) "quit")))
	 nil)
      (cond
	((and (> (length line) 5) (equal (subseq line 0 4) "say "))
	 (maphash (lambda (c n)
		    (declare (ignore n))
		    (format (socket-stream c) "~a: ~a~%" name (subseq line 4 (length line)))
		    (force-output (socket-stream c)))
		  *connections*))
	((and (>= (length line) 7) (equal (subseq line 0 7) "players"))
	 (show-players name))
	((and (>= (length line) 4) (equal (subseq line 0 4) "hand"))
	 (show-player-hand name))
	((and (>= (length line) 3) (equal (subseq line 0 3) "top"))
	 (show-top-card name))
	((and (>= (length line) 4) (equal (subseq line 0 4) "turn"))
	 (show-current-turn name))
	((and (> (length line) 5) (equal (subseq line 0 5) "play "))
	 (if (= (length *players*) 1)
	     (progn
	       (format (socket-stream conn) "Wait until there are at least 2 players~%")
	       (force-output (socket-stream conn)))
	     (let* ((card-number (handler-case (1- (parse-integer (subseq line 5)))
				   (error (err)
				     (declare (ignore err))
				     (format (socket-stream conn) "Invalid card number~%")
				     (force-output (socket-stream conn))
				     nil)))
		    (card (when card-number
			    (let ((hand (get-hand name)))
			      (if (or (< card-number 0) (>= card-number (length hand)))
				  (progn
				    (format (socket-stream conn) "Invalid card number~%")
				    (force-output (socket-stream conn)))
				  (nth card-number hand))))))
	       (when card
		 (cond
		   ((not (equal (current-player) name))
		    (format (socket-stream conn) "It's not your turn!~%")
		    (force-output (socket-stream conn)))
		   ((not (card-playable-p card *top-card*))
		    (format (socket-stream conn) "You can't play a ~a on a ~a~%"
			    (card-to-string card) (card-to-string *top-card*))
		    (force-output (socket-stream conn)))
		   (t (play-card name card)
		      (let ((type (car *top-card*)))
			(cond
			  ((equal type "draw4")
			   (handle-draw4 name))
			  ((equal type "change color")
			   (handle-change-color name))
			  (t (announce-turn))))))))))
	(t
	 (progn
	   (format (socket-stream conn) "~a~%" line)
	   (force-output (socket-stream conn))))))
    (deinit-connection conn name)))

(defun accept-connections (sock)
  (do ((new-conn (socket-accept sock :element-type 'character)
		 (socket-accept sock :element-type 'character)))
      ((null new-conn) nil)
    (make-thread (lambda () (handle-connection new-conn)))))

(defun start-netuno ()
  (let* ((socket (socket-listen *address* *port*)))
    (setf *socket* socket)
    (setf *listen-thread*
	  (make-thread
	   (lambda () (accept-connections socket))
	   :name "listen-thread"))))

(defun stop-netuno ()
  (maphash (lambda (c name) (declare (ignore name)) (socket-close c)) *connections*)
  (setf *connections* (make-hash-table :test 'equal))
  (setf *player-conns* (make-hash-table :test 'equal))
  (setf *players* '())
  (socket-close *socket*)
  (destroy-thread *listen-thread*)
  (setf *listen-thread* nil)
  (setf *socket* nil))
