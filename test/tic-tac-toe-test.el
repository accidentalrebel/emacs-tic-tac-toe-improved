;;; Test for `tic-tac-toe'

;;; Commentary:
;; These are the tests for `tic-tac-toe'

;;; Code:

(ert-deftest coorder-initialize-area ()
  (with-temp-buffer
    (coorder-initialize-area 3 3)
    (should
     (equal
      (buffer-string) "   \n   \n   "
      )))
  (with-temp-buffer
    (coorder-initialize-area 1 1)
    (should
     (equal
      (buffer-string) " "))
    )
  (with-temp-buffer
    (coorder-initialize-area 0 1)
    (should
     (equal
      (buffer-string) ""))
    )
  )
