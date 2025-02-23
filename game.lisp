(in-package :netuno)

(defvar *players* '())

(defun current-player ()
  "The player whose turn this is"
  (car players))

(defun next-turn ()
  "Choose the next player"
  (append *players* (pop players)))

(defun reverse-order ()
  "Reverse the order, this should be called instead of next-turn after a reverse card"
  (setf *players* (reverse *players*)))
