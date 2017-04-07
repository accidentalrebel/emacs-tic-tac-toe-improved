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
Coordinates use an index of 0."
  (goto-line (point-min))
  (message "placing at %s, %s" col row)
  (dotimes (y row)
    (next-line 1)
    )
  (dotimes (x col)
    (right-char 1)
    )
  (delete-char 1)
  (insert char)
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
    (goto-line (point-min))
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
  )

;; HELPERS 
;;
(defun tic-tac-toe-place ()
  "Place"
  (interactive)
  (message "placing")
  (let ((inhibit-read-only t))
    (coorder-place-char-at (coorder-current-col) (coorder-current-row) "x")
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

