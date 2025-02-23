(in-package :netuno)

(defvar *colors* '("red" "yellow" "green" "blue"))

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
    (t (append (make-deck) (make-decks (1- n))))))

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

(defun card-playable-p (card top-card)
  "Whether this card is playable on the top card"
  (or (equal (cdr card) "any")
      (equal (cdr card) (cdr top-card))
      (equal (car card) (car top-card))))

(defun cards-by-colour-p (card1 card2)
  "Compares two cards by their colour, in alphabetical order"
  (char< (aref card1 0) (aref card2 0)))

(defun draw-card (deck hand)
  "Draws a card and sorts the hand, returns the new hand"
  (push (pop deck) hand)
  (sort hand #'cards-by-colour-p))

(defun draw-n-cards (deck hand n)
  "Draws n cards sorting the hand, returns the hand"
  (if (<= n 0)
      hand
      (draw-n-cards deck (draw-card deck hand) (1- n))))
