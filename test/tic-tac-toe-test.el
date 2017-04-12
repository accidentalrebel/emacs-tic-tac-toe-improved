;;; Test for `tic-tac-toe'

;;; Commentary:
;; These are the tests for `tic-tac-toe'

;;; Code:

(ert-deftest coorder-initialize-view-area ()
  (with-temp-buffer
    (coorder-initialize-view-area 3 3)
    (should
     (equal
      (buffer-string) "   \n   \n   ")))
  (with-temp-buffer
    (coorder-initialize-view-area 1 1 "x")
    (should
     (equal
      (buffer-string) "x")))
  (with-temp-buffer
    (coorder-initialize-view-area 0 1)
    (should
     (equal
      (buffer-string) ""))))

(ert-deftest coorder-place-char-at ()
  (with-temp-buffer
   (coorder-initialize-view-area 3 3 "x")
   (coorder-place-char-at 0 0 "a")
   (coorder-place-char-at 1 1 "b")
   (coorder-place-char-at 2 2 "c")
   (should
    (equal
     (buffer-string) "axx\nxbx\nxxc"))))

(ert-deftest coorder-place-string-at-area ()
  (with-temp-buffer
    (coorder-initialize-view-area 4 4 "-")
    (coorder-place-string-at-area 1 1 "xo\n x
xo")
    (should
     (equal (buffer-string) "----\n-xo-\n- x-\n-xo-"))))

(ert-deftest coorder-get-char-at ()
  (with-temp-buffer
    (coorder-initialize-view-area 3 3 "x")
    (coorder-place-char-at 0 0 "a")
    (coorder-place-char-at 1 1 "b")
    (coorder-place-char-at 2 2 "c")
    (should (equal (coorder-get-char-at 0 0) "a"))
    (should (equal (coorder-get-char-at 1 1) "b"))
    (should (equal (coorder-get-char-at 2 2) "c"))
    (should (equal (coorder-get-char-at 2 0) "x"))))

(ert-deftest tic-tac-toe--get-set-colors ()
  (with-temp-buffer
    (coorder-initialize-view-area 1 1 "x")
    (coorder-set-color-at 0 0 'green 'black)
    (should (equal (coorder-get-color-at 0 0) '(:background "green" :foreground "black")))
    (should (equal (coorder-get-bg-color-at 0 0) "green"))
    (should (equal (coorder-get-fg-color-at 0 0) "black"))

    (coorder-reset-color-at 0 0)
    (coorder-set-bg-color-at 0 0 'green)
    (should (equal (coorder-get-color-at 0 0) '(:background "green")))
    (should (equal (coorder-get-bg-color-at 0 0) "green"))
    (should (equal (coorder-get-fg-color-at 0 0) nil))

    (coorder-reset-color-at 0 0)
    (coorder-set-fg-color-at 0 0 'black)
    (should (equal (coorder-get-color-at 0 0) '(:foreground "black")))
    (should (equal (coorder-get-bg-color-at 0 0) nil))
    (should (equal (coorder-get-fg-color-at 0 0) "black"))
    ))

(ert-deftest tic-tac-toe--switch-to-next-player ()
  (with-temp-buffer
    (setq-local tic-tac-toe--current-player-number 1)
    (should (equal (tic-tac-toe--switch-to-next-player) 2))
    (should (equal (tic-tac-toe--switch-to-next-player) 1))
    (should (equal (tic-tac-toe--switch-to-next-player) 2))
    (setq-local tic-tac-toe--current-player-number 3)
    (should (equal (tic-tac-toe--switch-to-next-player) 1))
    (setq-local tic-tac-toe--current-player-number 1)
    (should (equal (tic-tac-toe--switch-to-next-player) 2))))

(ert-deftest tic-tac-toe--get-current-symbol ()
  (with-temp-buffer
    (setq-local tic-tac-toe--current-player-number 1)
    (should (equal (tic-tac-toe--get-current-symbol) "x"))
    (setq-local tic-tac-toe--current-player-number 2)
    (should (equal (tic-tac-toe--get-current-symbol) "o"))))

(ert-deftest tic-tac-toe--check-winner ()
  (with-temp-buffer
    ;; FIRST PLAYER
    (setq-local tic-tac-toe--current-player-number 1)
    (let ((cols (car tic-tac-toe--view-area-size))
	  (rows (car (cdr tic-tac-toe--view-area-size))))

      (erase-buffer)			;
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 2 "x")
      (coorder-place-char-at 3 2 "x")
      (coorder-place-char-at 4 2 "x")
      (should (tic-tac-toe--check-winner))

      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 3 "x")
      (coorder-place-char-at 3 3 "x")
      (coorder-place-char-at 4 3 "x")
      (should (tic-tac-toe--check-winner))

      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 4 "x")
      (coorder-place-char-at 3 4 "x")
      (coorder-place-char-at 4 4 "x")
      (should (tic-tac-toe--check-winner))

      ;; SECOND PLAYER
      (setq-local tic-tac-toe--current-player-number 2)

      (erase-buffer)			;
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 2 "o")
      (coorder-place-char-at 3 2 "o")
      (coorder-place-char-at 4 2 "o")
      (should (tic-tac-toe--check-winner))

      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 3 "o")
      (coorder-place-char-at 3 3 "o")
      (coorder-place-char-at 4 3 "o")
      (should (tic-tac-toe--check-winner))

      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 4 "o")
      (coorder-place-char-at 3 4 "o")
      (coorder-place-char-at 4 4 "o")
      (should (tic-tac-toe--check-winner))

      ;; VERTICAL
      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 3 2 "o")
      (coorder-place-char-at 3 3 "o")
      (coorder-place-char-at 3 4 "o")
      (should (tic-tac-toe--check-winner))

      ;; DIAGONAL
      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 2 "o")
      (coorder-place-char-at 3 3 "o")
      (coorder-place-char-at 4 4 "o")
      (should (tic-tac-toe--check-winner))

      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 4 "o")
      (coorder-place-char-at 3 3 "o")
      (coorder-place-char-at 4 2 "o")
      (should (tic-tac-toe--check-winner))

      ;; NON-MATCHING
      (erase-buffer)
      (coorder-initialize-view-area cols rows " ")
      (should (not (tic-tac-toe--check-winner)))

      (erase-buffer)			;
      (coorder-initialize-view-area cols rows " ")
      (coorder-place-char-at 2 2 "x")
      (coorder-place-char-at 3 2 "o")
      (coorder-place-char-at 4 2 "x")
      (should (not (tic-tac-toe--check-winner)))
      )))
