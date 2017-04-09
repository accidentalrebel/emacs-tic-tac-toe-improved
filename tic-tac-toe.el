;;; tic-tac-toe.el --- Tic Tac Toe Game                  -*- lexical-binding: t -*-

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
(defconst tic-tac-toe--player-symbols '("x" "o")
  "Symbols to be used by players.")

(defconst tic-tac-toe--board-start-coordinate '(2 2)
  "Starting coodinate for the board.")

(defconst tic-tac-toe--view-area-size '(30 12)
  "The size of the view area.")

(defvar tic-tac-toe--current-player-number 1 "Number of the current player.")

(defface tic-tac-toe--win-face '((t . (:background "green" :foreground "black"))) "Test Face" :group 'tic-tac-toe-faces)

(define-derived-mode tic-tac-toe-mode special-mode "tic-tac-toe-mode")

(defun tic-tac-toe--control-config ()
  "Initial config for setting of controls."
  (local-set-key (kbd "C-c SPC") 'tic-tac-toe-place))

(add-hook 'tic-tac-toe-mode-hook 'tic-tac-toe--control-config)

;; COORDER
;;
(defun coorder-initialize-view-area (cols rows &optional char)
  "Initialize an area for drawing.
COLS specify the number of columns.
ROWS specify the number of rows.
[optional] CHAR, the char to place."
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
  (save-excursion
    (coorder-position-point-at col row)
    (replace-rectangle (point) (+ (point) 1) char)))

(defun coorder-place-char-at-area (col row width height char)
  "Place."
  (dotimes (x height)
    (dotimes (y width)
      (coorder-place-char-at (+ col x) (+ row y) char)
      )
    ))

(defun coorder-get-char-at (col row)
  "Gets the char at COL and ROW coordinates.
Coordinates use a starting index of 0."
  (coorder-position-point-at col row)
  (string (char-after)))

(defun coorder-position-point-at (col row)
  "Positions the point at COL and ROW coondinates.
Coordinates use a starting index of 0."
  (goto-char (point-min))
  (forward-line row)
  (move-to-column col))

(defun coorder-set-text-property-at (col row face)
  "Set the text property at COL and ROW with FACE."
  (coorder-position-point-at col row)
  (put-text-property (point) (+ (point) 1) 'font-lock-face face))

(defun coorder-set-color-at (col row bg-color fg-color)
  "Set the color at COL and ROW with BG-COLOR and FG-COLOR."
  (coorder-position-point-at col row)
  (let (property-list '())
    (when bg-color
      (setq property-list (plist-put property-list ':background (symbol-name bg-color))))
    (when fg-color
      (setq property-list (plist-put property-list ':foreground (symbol-name fg-color))))
	 
    (put-text-property (point) (+ (point) 1) 'font-lock-face property-list)))

(defun coorder-reset-color-at (col row)
  "Reset the color text property at COL and ROW."
  (coorder-position-point-at col row)
  (remove-text-properties (point) (+ (point) 1) '(font-lock-face)))

(defun coorder-set-bg-color-at (col row bg-color)
  "Set the color at COL and ROW with just the BG-COLOR."
  (coorder-set-color-at col row bg-color nil))

(defun coorder-set-fg-color-at (col row fg-color)
  "Set the color at COL and ROW with just the FG-COLOR."
  (coorder-set-color-at col row nil fg-color))

(defun coorder-get-color-at (col row)
  "Get the color at COL and ROW.
Returns (:background BG-COLOR :foreground FG-COLOR)"
  (coorder-position-point-at col row)
  (get-text-property (point) 'font-lock-face))

(defun coorder-get-bg-color-at (col row)
  "Gets the background color at COL and ROW."
  (plist-get (coorder-get-color-at col row) ':background))

(defun coorder-get-fg-color-at (col row)
  "Gets the foreground color at COL and ROW."
  (plist-get (coorder-get-color-at col row) ':foreground))

(defun coorder-current-col ()
  "Return the current col at point position.
Has an index of 0."
  (current-column))

(defun coorder-current-row ()
  "Return the current row at point position.
Has an index of 0."
  (- (line-number-at-pos) 1))

;; MAIN
;;
(defun tic-tac-toe--initialize-board ()
  "Initialize the board with a \"-\" character."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (coorder-initialize-view-area (car tic-tac-toe--view-area-size) (car (cdr tic-tac-toe--view-area-size)) " ")
    (coorder-place-char-at-area (car tic-tac-toe--board-start-coordinate) (car (cdr tic-tac-toe--board-start-coordinate)) 3 3 "-")
    ))

(defun tic-tac-toe-start ()
  "Start the game."
  (interactive)
  (other-window 1)
  (switch-to-buffer "*scratch*")
					;(buffer-disable-undo "*scratch*")
  (let ((inhibit-read-only t))
    (erase-buffer))

  (tic-tac-toe-mode)
  (tic-tac-toe--initialize-board)
  (coorder-position-point-at 3 3)
  (setq tic-tac-toe--current-player-number 1)
  (message "Start!"))

;; HELPERS
;;
(defun tic-tac-toe-place ()
  "Places a symbol on the current point position."
  (interactive)
  (let ((inhibit-read-only t)
	(current-symbol (tic-tac-toe--get-current-symbol)))
    (if (equal "-" (coorder-get-char-at (coorder-current-col) (coorder-current-row)))
	(progn
	  (coorder-place-char-at (coorder-current-col) (coorder-current-row) current-symbol)
	  (tic-tac-toe--check-winner)
	  (tic-tac-toe--switch-to-next-player))
      (tic-tac-toe--display-notif-message "Tile is already occupied!"))))

(defun tic-tac-toe--check-winner ()
  "Check if any player has already won."
  (save-excursion
    (let ((winning-coordinates (catch 'found-winner
				 (tic-tac-toe--get-winner-horizontally)
				 (tic-tac-toe--get-winner-vertically)
				 (tic-tac-toe--get-winner-diagonally))))
      (if winning-coordinates
	  (progn
	    (tic-tac-toe--highlight-winning-coordinates winning-coordinates)
	    (tic-tac-toe--on-found-winner)
	    t)
	nil))))

(defun tic-tac-toe--get-winner-horizontally ()
  "Check for a winner horizontally."
  (let ((has-won nil)
	(winning-coordinates ()))
    (dotimes (row 3)
      (setq has-won t)
      (setq winning-coordinates ())
      (dotimes (col 3)
	(when (not (equal (coorder-get-char-at col row) (tic-tac-toe--get-current-symbol)))
	  (setq has-won nil))
	(setq winning-coordinates (append winning-coordinates (list (list col row))))
	)
      (when has-won
	(throw 'found-winner winning-coordinates))
      )))

(defun tic-tac-toe--get-winner-vertically ()
  "Check for a winner horizontally."
  (let ((has-won nil)
	(winning-coordinates ()))
    (dotimes (col 3)
      (setq has-won t)
      (setq winning-coordinates ())
      (dotimes (row 3)
	(let ((current-col (+ col (car tic-tac-toe--board-start-coordinate)))
	      (current-row (+ row (car (cdr tic-tac-toe--board-start-coordinate)))))
	  (when (not (equal (coorder-get-char-at current-col current-row) (tic-tac-toe--get-current-symbol)))
	    (setq has-won nil))
	  (setq winning-coordinates (append winning-coordinates (list (list current-col current-row)))))
	)
      (when has-won
	(throw 'found-winner winning-coordinates))
      )))

(defun tic-tac-toe--get-winner-diagonally ()
  "Check for a winner diagonally."
  (let ((has-won t)
	(winning-coordinates ()))

    (dotimes (step 3)
      (when (not (equal (coorder-get-char-at step step) (tic-tac-toe--get-current-symbol)))
    	(setq has-won nil))
      (setq winning-coordinates (append winning-coordinates (list (list step step))))
      )
    (when has-won
      (throw 'found-winner winning-coordinates))

    (setq has-won t)
    (setq winning-coordinates ())
    (dotimes (step 3)
      (when (not (equal (coorder-get-char-at step (- 2 step)) (tic-tac-toe--get-current-symbol)))
    	(setq has-won nil))
      (setq winning-coordinates (append winning-coordinates (list (list step (- 2 step)))))
      )

    (when has-won
      (throw 'found-winner winning-coordinates))))


(defun tic-tac-toe--highlight-winning-coordinates (coordinates)
  "Highlight the winning COORDINATES."
  (dolist (coordinate coordinates)
    (coorder-set-color-at (car coordinate) (car (cdr coordinate)) 'green 'black)
    ))

;; DISPLAY
;;
(defun tic-tac-toe--display-notif-message (str)
  "Display STR at the dedicated notification area."
  (save-excursion
    (message "PLACING")
    (coorder-position-point-at 0 7)
    (replace-rectangle (point) (+ (point) (string-width str)) str)
    ))

;; EVENTS
;; 
(defun tic-tac-toe--on-found-winner ()
  "Handles what happens when someone wins."
  (tic-tac-toe--display-notif-message (concat "Player " (number-to-string tic-tac-toe--current-player-number) " wins!")))

(defun tic-tac-toe--get-current-symbol ()
  "Gets the current symbol for the current player."
  (nth (- tic-tac-toe--current-player-number 1) tic-tac-toe--player-symbols))

(defun tic-tac-toe--switch-to-next-player ()
  "Switch to the next player."
  (let ((new-player-number (+ tic-tac-toe--current-player-number 1)))
    (if (> new-player-number (length tic-tac-toe--player-symbols))
	(setq tic-tac-toe--current-player-number 1)
      (setq tic-tac-toe--current-player-number new-player-number))
    ))

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

