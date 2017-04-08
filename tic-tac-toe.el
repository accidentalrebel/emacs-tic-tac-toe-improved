;;; tic-tac-toe.el --- Tic Tac Toe Game              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  AccidentalRebel

;; Author: Karlo <accidentalrebel@gmail.com>
;; Keywords: games
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar tic-tac-toe--current-player-number 1 "Number of the current player.")

(defconst tic-tac-toe--player-symbols '("x" "o") "Symbols to be used by players.")

(define-derived-mode tic-tac-toe-mode special-mode "tic-tac-toe-mode")

(defun tic-tac-toe-config ()
  (local-set-key (kbd "C-c SPC") 'tic-tac-toe-place))

(add-hook 'tic-tac-toe-mode-hook 'tic-tac-toe-config)


;; COORDER
;;
(defun coorder-initialize-area (cols rows &optional char)
  "Initialize an area for drawing."
  (dotimes (row rows)
    (dotimes (col cols)
      (insert (if char
		  char
		" "))
      )
    (when (< row (- rows 1))
      (newline))
    ))

(defun coorder-place-char-at (col row char)
  "Place char at COL and ROW coordinates.
CHAR can be any value.
Coordinates use a starting index of 0."
  (coorder-position-point-at col row)
  (delete-char 1)
  (insert char)
  (left-char)
  )

(defun coorder-get-char-at (col row)
  "Gets the char at COL and ROW coordinates.
Coordinates use a starting index of 0."
  (coorder-position-point-at col row)
  (string (char-after)))

(defun coorder-position-point-at (col row)
  "Positions the point at COL and ROW coondinates.
Coordinates use a starting index of 0."
  (goto-char (point-min))
  (dotimes (y row)
    (next-line 1))
  (beginning-of-line)
  (dotimes (x col)
    (right-char 1))
  )

(defun coorder-current-col ()
  "Col. Index of 0."
  (current-column)
  )

(defun coorder-current-row ()
  "Row. index of 0."
  (- (line-number-at-pos) 1)
  )

;; MAIN
;;
(defun tic-tac-toe--initialize-board ()
  "Initialize the board with a \"-\" character."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (coorder-initialize-area 3 3 "-")
    )
  )

(defun tic-tac-toe-start ()
  "Start the game."
  (interactive)
  (other-window 1)
  (switch-to-buffer "*scratch*")
					;(buffer-disable-undo "*scratch*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    )
  (tic-tac-toe-mode)
  (tic-tac-toe--initialize-board)
  (setq tic-tac-toe--current-player-number 1)
  (message "Start!")
  )

;; HELPERS 
;;
(defun tic-tac-toe-place ()
  "Places a symbol on the current point position."
  (interactive)
  (let ((inhibit-read-only t)
	(current-symbol (tic-tac-toe--get-current-symbol)))
    (coorder-place-char-at (coorder-current-col) (coorder-current-row) current-symbol)
    (tic-tac-toe--check-winner)
    (tic-tac-toe--switch-to-next-player)
    )
  )

(defun tic-tac-toe--check-winner ()
  "Checks if someone already won."
  (save-excursion
    (let ((winning-coordinates (catch 'found-winner
				 (tic-tac-toe--get-winner-horizontally))))
      (if winning-coordinates
	  (progn 
	    (message "HERE WE GO: Winnig coordinate %s" winning-coordinates)
	    (tic-tac-toe--highlight-winning-coordinates winning-coordinates)
	    (tic-tac-toe--on-found-winner)
	    t)
	nil))))

(defun tic-tac-toe--get-winner-horizontally ()
  "Checks winner horizontally."
  (let ((has-won nil)
	(winning-coordinates ()))
    (dotimes (row 3)
      (setq has-won t)
      (setq winning-coordinates ())
      (dotimes (col 3 has-won)
	(when (not (equal (coorder-get-char-at col row) (tic-tac-toe--get-current-symbol)))
	  (setq has-won nil))
	(setq winning-coordinates (append winning-coordinates (list (list col row))))
	)
      (when has-won
	(throw 'found-winner winning-coordinates))
      )))

(defun tic-tac-toe--highlight-winning-coordinates (coordinates)
  (setq coordinates '((0 1) (1 1) (2 1)))
  (dolist (coordinate coordinates)
    (message "Checker: %s" (coorder-get-char-at (car coordinate) (car (cdr coordinate))))
    )
  )

;; EVENTS
;; 
(defun tic-tac-toe--on-found-winner ()
  "Handles what hoppens when someone wins."
  (message "~~~~~~~~~~~~~~~~~~~~~~~ Found!"))

(defun tic-tac-toe--get-current-symbol ()
  "Gets the current symbol for the current player."
  (nth (- tic-tac-toe--current-player-number 1) tic-tac-toe--player-symbols))

(defun tic-tac-toe--switch-to-next-player ()
  "Switch to the next player."
  (let ((new-player-number (+ tic-tac-toe--current-player-number 1)))
    (if (> new-player-number (length tic-tac-toe--player-symbols))
	(setq tic-tac-toe--current-player-number 1)
      (setq tic-tac-toe--current-player-number new-player-number))
    )
  )

(local-set-key (kbd "<f5>") (lambda ()
			      (interactive)
			      (save-buffer)
			      (eval-buffer)
			      (tic-tac-toe-start)))
(local-set-key (kbd "<f6>") (lambda ()
			      (interactive)
			      (shell-command "cask exec ert-runner")))

(provide 'tic-tac-toe)
;;; tic-tac-toe.el ends here

