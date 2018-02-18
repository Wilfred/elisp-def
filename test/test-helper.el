;;; test-helper.el --- Helpers for elisp-def-test.el

(require 'ert)
(require 'f)

(let ((elisp-def-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elisp-def-dir))

;;; test-helper.el ends here
