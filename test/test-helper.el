;;; test-helper.el --- Helpers for elisp-def-test.el

(require 'ert)
(require 'f)

(let ((elisp-def-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path elisp-def-dir))

(require 'undercover)
(undercover "elisp-def.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

;;; test-helper.el ends here
