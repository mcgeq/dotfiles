;;; run-tests.el --- Load MCG configuration test suites -*- lexical-binding: t; -*-

(load (expand-file-name "test-helper.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun mcg-test-suite-files (directory)
  "Return sorted *-tests.el suites in DIRECTORY, excluding this entrypoint."
  (cl-remove-if
   (lambda (file) (string= (file-name-nondirectory file) "run-tests.el"))
   (directory-files directory t "-tests\\.el\\'")))

(dolist (file (mcg-test-suite-files (expand-file-name "test" mcg-test-root)))
  (load file nil t))

;;; run-tests.el ends here
