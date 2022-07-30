;;; antics-test.el --- testing for antics.el

;;; Commentary:

;;; Code:
(load (expand-file-name "./antics.el"))

(defvar antics--testing t
  "Whether to execute tests while evaluating this file.")

(defun antics--tests-all ()
  "Run all antics tests."
  (when antics--configuration
    (message "exists")))

(when antics--testing
  (antics--tests-all))

;;; antics-test.el ends here
