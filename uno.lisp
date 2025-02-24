(in-package :netuno)

(defvar *colors* '("red" "yellow" "green" "blue"))
(defvar *players* '())
(defvar *hands* (make-hash-table :test 'equal))
(defvar *deck* '())
(defvar *top-card* nil)

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
  (let ((deck-count (ceiling (/ (length *players*) 4))))
    (setf *deck* (make-n-decks deck-count))
    (n-shuffle-deck *deck* 7)))

(defun init-top-card ()
  (when (= (length *deck*) 0)
    (init-deck))
  (setf *top-card* (pop *deck*))
  (when (equal (car *top-card*) "draw4")
    (setf *deck* (append *deck* *top-card*))
    (init-top-card)))

(defun init-game ()
  (init-deck)
  (init-top-card))

(defun card-playable-p (card top-card)
  "Whether this card is playable on the top card"
  (or (equal (cdr card) "any")
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

(defun draw-card (hand)
  "Draws a card and sorts the hand, returns the new hand"
  (when (= (length *deck*) 0)
    (init-deck))
  (push (pop *deck*) hand))

(defun draw-n-cards (hand n)
  "Draws n cards sorting the hand, returns the hand"
  (if (<= n 0)
      hand
      (draw-n-cards (draw-card hand) (1- n))))

(defun sort-cards (cards)
  "Sort a card by type, then colour"
  (sort (sort cards #'cards-by-type-p) #'cards-by-colour-p))

(defun current-player ()
  "The player whose turn this is"
  (car *players*))

(defun next-turn ()
  "Choose the next player"
  (append *players* (pop *players*)))

(defun reverse-order ()
  "Reverse the order, this should be called instead of next-turn after a reverse card"
  (setf *players* (reverse *players*)))

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
      (format nil "[~a]" (car card))
      (format nil "[~a ~a]" (cdr card) (car card))))

(defun hand-to-string (hand)
  "Transform a hand to a string"
  (when hand
    (string-trim " " (concatenate 'string (card-to-string (car hand)) " " (hand-to-string (cdr hand))))))

(defun add-player (name)
  "Add a player to the game"
  (when (= (length *players*) 0)
    (init-game))
  (setf *players* (append *players* (list name)))
  (set-hand name (draw-n-cards '() 7))
  (sort-hand name))

(defun remove-player (name)
  "Remove a player from the game"
  (setf *players* (remove name *players* :test #'equal))
  (remhash name *hands*))

(defun play-card (name card)
  "Plays the card from the player's hand and apply side effects"
  (let ((hand-length (length (get-hand name)))
	(new-hand (remove card (get-hand name) :test #'equal)))
    (when (and (card-playable-p card *top-card*)
	       (< (length new-hand) hand-length))
      (set-hand name new-hand)
      (cond
	;; draw4 and change color require interaction with the user
	;; and are left to the server
	((equal (car card) "draw2")
	 (progn
	   (let ((player (cadr *players*)))
	     (set-hand player (draw-n-cards (get-hand player) 2))
	     (next-turn))))
	((equal (car card) "skip")
	 (next-turn))
	((equal (car card) "reverse")
	 (reverse-order)))
      (when (not (equal (car card) "reverse"))
	(next-turn))
      (setf *top-card* card))))
