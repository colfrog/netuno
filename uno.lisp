(in-package :netuno)

(defvar *colors* '("red" "yellow" "green" "blue"))
(defvar *players* '())
(defvar *players-lock* (make-lock))
(defvar *hands* (make-hash-table :test 'equal))
(defvar *uno* (make-hash-table :test 'equal))
(defvar *deck* '())
(defvar *top-card* nil)

(setf sb-impl::*default-external-format* :utf-8)

(defvar +color-codes+ (make-hash-table :test 'equal))
(setf (gethash "red" +color-codes+) 91)
(setf (gethash "yellow" +color-codes+) 93)
(setf (gethash "green" +color-codes+) 92)
(setf (gethash "blue" +color-codes+) 94)

(defun make-deck ()
  "Make a standard uno deck"
  (let ((deck '()))
    (dolist (c *colors*)
      (when (not (equal c "any"))
	(dotimes (i 10)
	  (push (cons (write-to-string i) c) deck)
	  (when (/= i 0)
	    (push (cons (write-to-string i) c) deck)))
	(dotimes (i 2)
	  (push (cons "draw2" c) deck)
	  (push (cons "reverse" c) deck)
	  (push (cons "skip" c) deck))
	(push '("draw4" . "any") deck)
	(push '("change color" . "any") deck)))
    deck))

(defun make-n-decks (n)
  "Append n standard uno decks together"
  (cond
    ((<= n 0) nil)
    ((= n 1) (make-deck))
    (t (append (make-deck) (make-n-decks (1- n))))))

(defun shuffle-deck (deck)
  "Fisher-Yates shuffle in-place"
  (let ((n (length deck)))
    (dotimes (i n)
      (let* ((k (- n i 1))
	     (j (random (1+ k)))
	     (cardk (nth k deck))
	     (cardj (nth j deck)))
	(when (/= j k)
	  (setf (nth k deck) cardj)
	  (setf (nth j deck) cardk))))))

(defun n-shuffle-deck (deck n)
  "Shuffles the deck n times"
  (when (> n 0)
    (shuffle-deck deck)
    (n-shuffle-deck deck (1- n))))

(defun init-deck ()
  (let ((deck-count (max 1 (ceiling (/ (length *players*) 4)))))
    (setf *deck* (make-n-decks deck-count))
    (n-shuffle-deck *deck* 7)))

(defun init-top-card ()
  (when (= (length *deck*) 0)
    (init-deck))
  (let ((top-card (pop *deck*)))
    (if (equal (car top-card) "draw4")
	(progn
	  (setf *deck* (append *deck* (list top-card)))
	  (init-top-card))
	(play-card nil top-card))))

(defun init-game ()
  (init-deck)
  (init-top-card))

(defun reset-game (winner)
  (init-game)
  (do ((player (current-player) (current-player)))
      ((or (equal player winner) (null (find winner *players* :test 'equal))) t)
    (next-turn))
  (dolist (player *players*)
    (set-hand player (sort-cards (draw-n-cards 7)))))

(defun card-playable-p (card top-card)
  "Whether this card is playable on the top card"
  (or (equal (cdr card) "any")
      (equal (cdr top-card) "any")
      (equal (cdr card) (cdr top-card))
      (equal (car card) (car top-card))))

(defun cards-by-type-p (card1 card2)
  "Compares two cards by their type, in ASCII order"
  (let* ((type1 (car card1))
	 (type2 (car card2))
	 (mini (min (length type1) (length type2))))
  (do ((i 0 (1+ i)))
      ((or (char/= (aref type1 i) (aref type2 i)) (= i (1- mini)))
       (char<= (aref type1 i) (aref type2 i))))))

(defun cards-by-colour-p (card1 card2)
  "Compares two cards by their colour, in alphabetical order"
  (char< (aref (cdr card1) 0) (aref (cdr card2) 0)))

(defun draw-card ()
  "Draws a card"
  (when (= (length *deck*) 0)
    (init-deck))
  (pop *deck*))

(defun draw-n-cards (n)
  "Draws n cards and returns them"
  (when (> n 0)
    (cons
     (draw-card)
     (draw-n-cards (1- n)))))

(defun sort-cards (cards)
  "Sort a card by type, then colour"
  (sort (sort cards #'cards-by-type-p) #'cards-by-colour-p))

(defun draw-n-cards-and-print (name n &key (stream nil))
  "Draws n cards, prints them and adds them to the hand"
  (let ((cards (draw-n-cards n)))
    (when stream
      (format stream "You drew: ")
      (dolist (card cards)
	(format stream "~a " (card-to-string card)))
      (format stream "~c~%" #\Return)
      (force-output stream))
    (setf (gethash name *uno*) nil)
    (set-hand name (sort-cards (append cards (get-hand name))))
    cards))

(defun current-player ()
  "The player whose turn this is"
  (car *players*))

(defun next-turn ()
  "Choose the next player"
  (with-lock-held (*players-lock*)
    (when *players*
      (let ((player (pop *players*)))
	(setf *players* (append *players* (list player)))))))

(defun reverse-order ()
  "Reverse the order, this should be called instead of next-turn after a reverse card"
  (with-lock-held (*players-lock*)
    (setf *players* (reverse *players*))))

(defun get-hand (name)
  "Get a player's hand"
  (gethash name *hands*))

(defun set-hand (name value)
  "Set a player's hand"
  (setf (gethash name *hands*) value))

(defun sort-hand (name)
  "Sort a player's hand"
  (set-hand name (sort-cards (get-hand name))))

(defun card-to-string (card)
  "Transform a card to a string"
  (if (equal (cdr card) "any")
      (format nil "~c[95m[~a]~c[0m" #\Esc (car card) #\Esc)
      (format nil "~c[~dm[~a ~a]~c[0m"
	      #\Esc (gethash (cdr card) +color-codes+)
	      (cdr card) (car card) #\Esc)))

(defun hand-to-string (hand &optional (n 1))
  "Transform a hand to a string"
  (when hand
    (string-trim " " (concatenate 'string (write-to-string n) ":"
				  (card-to-string (car hand)) " "
				  (hand-to-string (cdr hand) (1+ n))))))

(defun add-player (name)
  "Add a player to the game"
  (when (= (length *players*) 0)
    (init-game))
  (with-lock-held (*players-lock*)
    (setf *players* (append *players* (list name))))
  (set-hand name (draw-n-cards 7))
  (sort-hand name))

(defun remove-player (name)
  "Remove a player from the game"
  (with-lock-held (*players-lock*)
    (setf *players* (remove name *players* :test #'equal)))
  (remhash name *hands*))

(defun play-card (name card &key (player-conns nil))
  "Plays the card from the player's hand and apply side effects"
  (let ((hand-length (if name (length (get-hand name)) 0))
	(new-hand (when name (remove card (get-hand name) :test #'equal :count 1))))
    (when (or (null name)
	      (and (card-playable-p card *top-card*)
		   (< (length new-hand) hand-length)))
      (when name
	(set-hand name new-hand))
      (cond
	;; draw4 and change color require interaction with the user
	;; and are left to the server
	((equal (car card) "draw2")
	 (let* ((player (if name (cadr *players*) (car *players*)))
		(conn (when player-conns (gethash player player-conns))))
	   (draw-n-cards-and-print player 2 :stream (when conn (socket-stream conn)))
	   (next-turn)))
	((equal (car card) "skip")
	 (next-turn))
	((equal (car card) "reverse")
	 (reverse-order)))
      (when (or (= (length *players*) 2) (not (equal (car card) "reverse")))
	(next-turn))
      (setf *top-card* card))))
