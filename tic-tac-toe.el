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

(require 'coordinate (locate-user-emacs-file "dev/coordinate/coordinate"))

(defconst tic-tac-toe--player-symbols '("x" "o")
  "Symbols to be used by players.")

(defconst tic-tac-toe--board-start-coordinate '(2 2)
  "Starting coodinate for the board.")

(defconst tic-tac-toe--view-area-size '(30 12)
  "The size of the view area.")

(defconst tic-tac-toe--board-border-sprite
  "╔═══╗
║   ║
║   ║
║   ║
╚═══╝"
 "The sprite of the border of the board.")

(defvar tic-tac-toe--current-player-number 1 "Number of the current player.")

(defvar tic-tac-toe--winner-player-number 0 "The player number of the winner.")

(defface tic-tac-toe--win-face '((t . (:background "green" :foreground "black"))) "Test Face" :group 'tic-tac-toe-faces)

(define-derived-mode tic-tac-toe-mode special-mode "tic-tac-toe-mode")

(defun tic-tac-toe--control-config ()
  "Initial config for setting of controls."
  (local-set-key (kbd "C-c SPC") 'tic-tac-toe-place))

(add-hook 'tic-tac-toe-mode-hook 'tic-tac-toe--control-config)

;; DEV ENVIRONMENT HELPER FUNCTIONS
;;
;; MAIN
;;
(defun tic-tac-toe--initialize-board ()
  "Initialize the board with a \"-\" character."
  (let ((inhibit-read-only t)
	(board-start-coordinate-x (car tic-tac-toe--board-start-coordinate))
	(board-start-coordinate-y (car (cdr tic-tac-toe--board-start-coordinate))))
    (goto-char (point-min))
    (coordinate-initialize-view-area (car tic-tac-toe--view-area-size) (car (cdr tic-tac-toe--view-area-size)) " ")
    (coordinate-place-string-at-area (- board-start-coordinate-x 1) (- board-start-coordinate-y 1) tic-tac-toe--board-border-sprite '(:background "brown"))
    (coordinate-place-char-at-area board-start-coordinate-x board-start-coordinate-y 3 3 "-")
    ))

;;;###autoload
(defun tic-tac-toe-start ()
  "Start the game."
  (interactive)

  (let ((inhibit-read-only t))
    ;; devenv-smart-open-elisp-output-window is a code in my emacs conf
    ;; What it does is that it handles where it would output the window
    ;; This does not run if you do not have this function.
    (if (fboundp 'devenv-smart-open-elisp-output-window)
	(devenv-smart-open-elisp-output-window "*tic-tac-toe*")
      (other-window 1)
      (switch-to-buffer "*tic-tac-toe*"))

    (erase-buffer)
    (tic-tac-toe-mode)
    (tic-tac-toe--initialize-board)
    (coordinate-position-point-at 3 3)
    (setq tic-tac-toe--current-player-number 1)
    (setq tic-tac-toe--winner-player-number 0)
    (tic-tac-toe--display-current-player)
    )
  (message "Start!"))

;; HELPERS
;;
(defun tic-tac-toe-place ()
  "Places a symbol on the current point position."
  (interactive)
  (when (= tic-tac-toe--winner-player-number 0)
      (let ((inhibit-read-only t)
	    (current-symbol (tic-tac-toe--get-current-symbol))
	    (char-at-point (coordinate-get-char-at (coordinate-current-col) (coordinate-current-row))))
	(if (equal "-" char-at-point)
	    (progn
	      (coordinate-place-char-at (coordinate-current-col) (coordinate-current-row) current-symbol)
	      (unless (or (tic-tac-toe--check-and-handle-winner)
			  (tic-tac-toe--check-and-handle-if-board-full))
		(tic-tac-toe--switch-to-next-player)
		(tic-tac-toe--display-current-player)))
	  (if (or (equal (car tic-tac-toe--player-symbols) char-at-point) (equal (car (cdr tic-tac-toe--player-symbols)) char-at-point))
	      (tic-tac-toe--display-notif-message "Tile is already occupied!")
	    (tic-tac-toe--display-notif-message "Cannot place there!")
	    (message "Text property: %s" (coordinate-get-text-property-at (coordinate-current-col) (coordinate-current-row)))
	    )))))

(defun tic-tac-toe--check-and-handle-if-board-full ()
  "Check if the board is full."
  (save-excursion
    (let ((start-col (car tic-tac-toe--board-start-coordinate))
	  (start-row (car (cdr tic-tac-toe--board-start-coordinate)))
	  (is-full t))
      (dotimes (row 3)
	(dotimes (col 3)
	  (when (and is-full (string= "-" (coordinate-get-char-at (+ start-col col) (+ start-row row))))
	    (setq is-full nil))
	  )
	)
      (if is-full
	  (progn
	    (tic-tac-toe--display-notif-message "Board is full!")
	    t)
	nil))))

(defun tic-tac-toe--check-and-handle-winner ()
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
	(let ((current-col (+ col (car tic-tac-toe--board-start-coordinate)))
	      (current-row (+ row (car (cdr tic-tac-toe--board-start-coordinate)))))
	  (when (not (equal (coordinate-get-char-at current-col current-row) (tic-tac-toe--get-current-symbol)))
	    (setq has-won nil))
	  (setq winning-coordinates (append winning-coordinates (list (list current-col current-row))))
	))
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
	  (when (not (equal (coordinate-get-char-at current-col current-row) (tic-tac-toe--get-current-symbol)))
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
      (let ((current-col (+ step (car tic-tac-toe--board-start-coordinate)))
	    (current-row (+ step (car (cdr tic-tac-toe--board-start-coordinate)))))
	
	(when (not (equal (coordinate-get-char-at current-col current-row) (tic-tac-toe--get-current-symbol)))
	  (setq has-won nil))
	(setq winning-coordinates (append winning-coordinates (list (list current-col current-row)))))
      )
    (when has-won
      (throw 'found-winner winning-coordinates))

    (setq has-won t)
    (setq winning-coordinates ())
    (dotimes (step 3)
      (let ((current-col (+ step (car tic-tac-toe--board-start-coordinate)))
	    (current-row (+ (- 2 step) (car (cdr tic-tac-toe--board-start-coordinate)))))
	(when (not (equal (coordinate-get-char-at current-col current-row) (tic-tac-toe--get-current-symbol)))
	  (setq has-won nil))
	(setq winning-coordinates (append winning-coordinates (list (list current-col current-row)))))
      )

    (when has-won
      (throw 'found-winner winning-coordinates))))


(defun tic-tac-toe--highlight-winning-coordinates (coordinates)
  "Highlight the winning COORDINATES."
  (dolist (coordinate coordinates)
    (coordinate-set-text-property-at (car coordinate) (car (cdr coordinate)) '(:foreground "black" :background "green"))
    ))

;; DISPLAY
;;
(defun tic-tac-toe--display-notif-message (str)
  "Display STR at the dedicated notification area."
  (save-excursion
    (coordinate-place-char-at-area 0 8 (car tic-tac-toe--view-area-size) 1 " ")
    (coordinate-place-string-at-area 1 8 str)))

(defun tic-tac-toe--display-current-player ()
  "Displays the current player on screen."
  (save-excursion
    (coordinate-place-char-at-area 0 7 (car tic-tac-toe--view-area-size) 1 " ")
    (coordinate-place-string-at-area 1 7 (concat "Current player: " (number-to-string  tic-tac-toe--current-player-number) "(" (tic-tac-toe--get-current-symbol) ")" ))))

;; EVENTS
;; 
(defun tic-tac-toe--on-found-winner ()
  "Handles what happens when someone wins."
  (setq tic-tac-toe--winner-player-number tic-tac-toe--current-player-number)
  (tic-tac-toe--display-notif-message (concat "Player " (number-to-string tic-tac-toe--winner-player-number) " wins!")))

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

;; Settings for dev environment
;;; This calls a code in my emacs conf that sets f5 and f6 keys for quick building.
;;; This wont run if you do not have this function.
(when (fboundp  'devenv-setup-build-keys)
  (devenv-setup-build-keys 'tic-tac-toe-start))

(provide 'tic-tac-toe)
;;; tic-tac-toe.el ends here

