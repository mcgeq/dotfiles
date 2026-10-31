;;; cumulative-workflow-smoke.el --- Real command workflow smoke -*- lexical-binding: t; -*-

(require 'cl-lib)
(load (expand-file-name "../scripts/profile-startup.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(let ((storage (make-temp-file "mcg-workflow" t)))
  (unwind-protect
      (progn
        (mcg-org-set-storage-root storage)
        (setq mcg-org-agenda-include-diary nil
              org-capture-bookmark nil
              org-capture-templates
              `(("z" "Review capture" entry (file ,mcg-org-task-file)
                 "* TODO Review Capture\n" :immediate-finish t)))
        (with-temp-file mcg-org-task-file (insert "* TODO Review Agenda\n"))
        (switch-to-buffer (get-buffer-create "*MCG Workflow*"))
        (execute-kbd-macro (kbd "C-c n c z"))
        (unless (mcg-module-loaded-p :org 'org)
          (error "Capture key did not initialize the Org module"))
        (with-temp-buffer
          (insert-file-contents mcg-org-task-file)
          (unless (search-forward "Review Capture" nil t)
            (error "Capture key did not save the selected private template")))
        (execute-kbd-macro (kbd "C-c n a t"))
        (with-current-buffer (window-buffer (selected-window))
          (unless (and (derived-mode-p 'org-agenda-mode)
                       (string-match-p "Review Agenda" (buffer-string))
                       (string-match-p "Review Capture" (buffer-string)))
            (error "Agenda did not include both temporary tasks")))
        (dolist (entry '(("review.md" :writing markdown)
                         ("review.py" :lang python)))
          (let ((file (expand-file-name (car entry) storage)))
            (with-temp-file file (insert ""))
            (let ((buffer (find-file-noselect file)))
              (with-current-buffer buffer
                (when (eq major-mode 'fundamental-mode)
                  (error "File trigger did not choose a mode for %s" file))))
            (unless (mcg-module-loaded-p (nth 1 entry) (nth 2 entry))
              (error "File trigger did not record %S" entry))))
        (when mcg-module-load-errors
          (error "Workflow module errors: %S" mcg-module-load-errors))
        (princ "Capture and Agenda keyboard workflows, Markdown/Python file triggers: passed\n"))
    (when (timerp mcg-idle-load-timer) (cancel-timer mcg-idle-load-timer))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name (file-in-directory-p buffer-file-name storage))
          (set-buffer-modified-p nil)
          (kill-buffer buffer))))
    (delete-directory storage t)))
