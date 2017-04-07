;;; test-helper --- Test helper for tic-tac-toe

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar tic-tac-toe-test-path
  (f-dirname (f-this-file)))

(defvar tic-tac-toe-root-path
  (f-parent tic-tac-toe-test-path))

(defvar tic-tac-toe-sandbox-path
  (f-expand "sandbox" tic-tac-toe-test-path))

(when (f-exists? tic-tac-toe-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" tic-tac-toe-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory tic-tac-toe-sandbox-path))
     (when (f-exists? tic-tac-toe-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir tic-tac-toe-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'tic-tac-toe)

(provide 'test-helper)
;;; test-helper.el ends here
