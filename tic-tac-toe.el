;;; tic-tac-toe.el --- Tic Tac Toe Game              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  AccidentalRebel

;; Author: Karlo <accidentalrebel@gmail.com>
;; Keywords: games

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
Coordinates use an index of 0."
  )

(defun tic-tac-toe--initialize-board ()
  "Initialize the board with a \"-\" character."
  (goto-line (point-min))
  (coorder-initialize-area 3 3 "-")
  )

(defun tic-tac-toe-start ()
  "Start the game."
  (interactive)
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (erase-buffer)
  (tic-tac-toe--initialize-board)
  )

(local-set-key (kbd "<f5>") (lambda ()
			      (interactive)
			      (save-buffer)
			      (eval-buffer)
			      (tic-tac-toe-start)))

(provide 'tic-tac-toe)
;;; tic-tac-toe.el ends here

