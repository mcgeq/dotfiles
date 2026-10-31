;;; build-entry-tests.el --- Test and build entrypoint regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(ert-deftest mcg-test-entrypoint-discovers-suites-in-order ()
  (let ((directory (make-temp-file "mcg-test-suites" t)))
    (unwind-protect
        (progn
          (dolist (name '("z-tests.el" "a-tests.el" "run-tests.el"
                          "test-helper.el" "startup-smoke.el"))
            (write-region "" nil (expand-file-name name directory)))
          (should (equal (mapcar #'file-name-nondirectory
                                 (mcg-test-suite-files directory))
                         '("a-tests.el" "z-tests.el"))))
      (delete-directory directory t))))

(ert-deftest mcg-test-helper-resolves-root-from-its-file ()
  (let ((default-directory temporary-file-directory))
    (load (expand-file-name "test/test-helper.el" mcg-emacs-dir) nil t)
    (should (file-exists-p (expand-file-name "config-org/init.org" mcg-test-root)))
    (should (equal (file-truename mcg-test-root)
                   (file-truename mcg-emacs-dir)))))

(defun mcg-test-make-dry-run (&rest arguments)
  "Return successful Make dry-run output for ARGUMENTS."
  (let ((default-directory mcg-emacs-dir))
    (with-temp-buffer
      (should (zerop (apply #'call-process "make" nil t nil "-n" arguments)))
      (buffer-string))))

(ert-deftest mcg-generate-stays-in-repository-and-does-not-create-private-config ()
  (let ((output (mcg-test-make-dry-run "-B" "generate")))
    (should (string-match-p "config-org/core/core-org.org" output))
    (should (string-match-p "config-org/modules/org/init-org.org" output))
    (should (string-match-p "site-lisp/config/early-init.el" output))
    (should-not (string-match-p "Copy-Item\\|cp -f\\|APPDATA\\|~/.emacs.d" output))
    (should-not (string-match-p "Put machine-local settings and secrets here" output))))

(ert-deftest mcg-make-test-generates-config-before-running-ert ()
  (let* ((output (mcg-test-make-dry-run "-B" "test"))
         (generation (string-match "config-org/core/core-org.org" output))
         (execution (string-match "-l test/run-tests.el" output)))
    (should generation)
    (should execution)
    (should (< generation execution))))

(ert-deftest mcg-tangle-dir-uses-the-shared-directory-rules ()
  (let ((output (mcg-test-make-dry-run "-B" "tangle-dir" "DIR=modules/org")))
    (should (string-match-p "config-org/modules/org/init-org.org" output))
    (should-not (string-match-p "for file in" output))
    (should-not (string-match-p "overriding recipe" output))))

;;; build-entry-tests.el ends here
