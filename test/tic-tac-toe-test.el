;;; Test for `tic-tac-toe'

;;; Commentary:
;; These are the tests for `tic-tac-toe'

;;; Code:

(ert-deftest coorder-initialize-area ()
  (with-temp-buffer
    (coorder-initialize-area 3 3)
    (should
     (equal
      (buffer-string) "   \n   \n   ")))
  (with-temp-buffer
    (coorder-initialize-area 1 1 "x")
    (should
     (equal
      (buffer-string) "x")))
  (with-temp-buffer
    (coorder-initialize-area 0 1)
    (should
     (equal
      (buffer-string) ""))))

(ert-deftest coorder-place-char-at ()
  (with-temp-buffer
   (coorder-initialize-area 3 3 "x")
   (coorder-place-char-at 0 0 "a")
   (coorder-place-char-at 1 1 "b")
   (coorder-place-char-at 2 2 "c")
   (should
    (equal
     (buffer-string) "axx\nxbx\nxxc"))))

(ert-deftest tic-tac-toe--switch-to-next-player ()
  (with-temp-buffer
    (setq-local tic-tac-toe--current-player-number 1)
    (should (equal (tic-tac-toe--switch-to-next-player) 2))
    (should (equal (tic-tac-toe--switch-to-next-player) 1))
    (should (equal (tic-tac-toe--switch-to-next-player) 2))
    (setq-local tic-tac-toe--current-player-number 3)
    (should (equal (tic-tac-toe--switch-to-next-player) 1))
    (setq-local tic-tac-toe--current-player-number 0)
    (should (equal (tic-tac-toe--switch-to-next-player) 1))))

(ert-deftest tic-tac-toe--get-current-symbol ()
  (with-temp-buffer
    (setq-local tic-tac-toe--current-player-number 1)
    (should (equal (tic-tac-toe--get-current-symbol) "x"))
    (setq-local tic-tac-toe--current-player-number 2)
    (should (equal (tic-tac-toe--get-current-symbol) "o"))))
