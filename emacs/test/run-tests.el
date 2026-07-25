;;; run-tests.el --- Tests for MCG Emacs config -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "site-lisp/config/core" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/editor" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/completion" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/tools" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/enhance" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/org" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/ui" default-directory))

(defvar mcg-emacs-dir default-directory)
(defvar mcg-extensions-dir (expand-file-name "site-lisp/extensions" default-directory))
(defvar mcg-org-directory (expand-file-name "org" temporary-file-directory))
(defvar mcg-org-agenda-directory (expand-file-name "agenda" mcg-org-directory))
(defvar mcg-org-notes-directory (expand-file-name "notes" mcg-org-directory))
(defvar mcg-org-default-notes-file (expand-file-name "notes.org" mcg-org-notes-directory))
(defvar mcg-org-task-file (expand-file-name "tasks.org" mcg-org-agenda-directory))

(require 'core-modules)
(require 'init-org-capture)

(defun mcg-test-capture-template (key)
  "Return capture template matching KEY."
  (assoc key org-capture-templates))

(defun mcg-test-capture-template-prop (key prop)
  "Return PROP from capture template KEY."
  (plist-get (nthcdr 5 (mcg-test-capture-template key)) prop))

(ert-deftest mcg-org-capture-slugify-keeps-readable-cjk-and-removes-invalid-chars ()
  (should (equal (mcg-org-capture-slugify-filename "我的 笔记: draft?*")
                 "我的-笔记-draft")))

(ert-deftest mcg-org-capture-build-file-uses-year-month-and-input-name ()
  (let ((path (mcg-org-capture-build-file
               (expand-file-name "notes/capture" temporary-file-directory)
               "2026-07-12 15:30:45"
               "Fallback Title"
               "Custom Name")))
    (should (string-suffix-p "notes/capture/2026/07/custom-name.org"
                             (replace-regexp-in-string "\\\\" "/" path)))))

(ert-deftest mcg-org-capture-build-file-falls-back-to-title-and-timestamp ()
  (let ((path (mcg-org-capture-build-file
               (expand-file-name "notes/capture" temporary-file-directory)
               "2026-07-12 15:30:45"
               "我的笔记"
               "")))
    (should (string-suffix-p "notes/capture/2026/07/20260712-153045-我的笔记.org"
                             (replace-regexp-in-string "\\\\" "/" path)))))

(ert-deftest mcg-org-capture-build-file-avoids-existing-file ()
  (let* ((root (make-temp-file "mcg-capture-test" t))
         (first (mcg-org-capture-build-file root "2026-07-12 15:30:45" "Title" "same")))
    (make-directory (file-name-directory first) t)
    (write-region "" nil first)
    (should (string-suffix-p "/2026/07/same-2.org"
                             (replace-regexp-in-string "\\\\" "/"
                                                       (mcg-org-capture-build-file
                                                        root "2026-07-12 15:30:45" "Title" "same"))))))

(ert-deftest mcg-org-set-storage-root-refreshes-all-org-capture-paths ()
  (let ((root (make-temp-file "mcg-org-root" t)))
    (mcg-org-set-storage-root root)
    (should (equal (file-name-as-directory root)
                   (file-name-as-directory mcg-org-directory)))
    (should (equal (expand-file-name "agenda" root)
                   mcg-org-agenda-directory))
    (should (equal (expand-file-name "notes" root)
                   mcg-org-notes-directory))
    (should (equal (expand-file-name "tasks.org" mcg-org-agenda-directory)
                   mcg-org-task-file))
    (should (equal (expand-file-name "diary.org" mcg-org-agenda-directory)
                   mcg-org-diary-file))
    (should (equal (expand-file-name "billing.org" mcg-org-notes-directory)
                   mcg-org-billing-file))
    (should (equal (expand-file-name "passwords.org" mcg-org-notes-directory)
                   mcg-org-password-file))
    (should (equal (expand-file-name "capture" mcg-org-notes-directory)
                   mcg-org-capture-notes-directory))
    (should (equal (expand-file-name "journal" mcg-org-notes-directory)
                   mcg-org-capture-journal-directory))
    (should (equal (list mcg-org-agenda-directory)
                   mcg-org-agenda-files))))

(ert-deftest mcg-org-storage-root-helper-is-available-before-idle-org-loads ()
  (let ((core-source (expand-file-name "config-org/core/core-lib.org"
                                       mcg-emacs-dir))
        (capture-source (expand-file-name "config-org/modules/org/init-org-capture.org"
                                          mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents core-source)
      (should (search-forward "(defun mcg-org-set-storage-root" nil t)))
    (with-temp-buffer
      (insert-file-contents capture-source)
      (should-not (search-forward "(defun mcg-org-set-storage-root" nil t)))))

(ert-deftest mcg-org-hugo-defaults-to-opt-in ()
  (let ((source (expand-file-name "config-org/modules/org/init-org-hugo.org"
                                  mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "(defcustom mcg-org-hugo-enable nil" nil t))))
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (should-not (default-value 'mcg-org-hugo-enable)))

(ert-deftest mcg-org-hugo-templates-are-opt-in-after-base-capture-templates ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let ((org-capture-templates
        (cl-remove-if (lambda (template)
                         (member (car-safe template)
                                 '("h" "hb" "hl" "hla" "hle" "hlr" "hlv" "he")))
                       (copy-tree org-capture-templates)))
        (mcg-org-hugo-enable t))
    (mcg-org-hugo-setup-capture-templates)
    (should (assoc "n" org-capture-templates))
    (should (assoc "j" org-capture-templates))
    (should (assoc "h" org-capture-templates))
    (should (assoc "hb" org-capture-templates))
    (dolist (key '("hl" "hla" "hle" "hlr" "hlv" "he"))
      (should-not (assoc key org-capture-templates)))))

(ert-deftest mcg-org-capture-editing-templates-jump-to-captured-files ()
  (should (mcg-test-capture-template-prop "n" :jump-to-captured))
  (should (mcg-test-capture-template-prop "j" :jump-to-captured)))

(ert-deftest mcg-org-capture-field-templates-finish-immediately ()
  (should (mcg-test-capture-template-prop "b" :immediate-finish))
  (should (mcg-test-capture-template-prop "p" :immediate-finish))
  (should (mcg-test-capture-template-prop "b" :kill-buffer))
  (should (mcg-test-capture-template-prop "p" :kill-buffer)))

(ert-deftest mcg-org-capture-state-is-reset-after-finalize ()
  (setq mcg-org-capture--last-title "Old title")
  (should (fboundp 'mcg-org-capture-reset-state))
  (should (memq #'mcg-org-capture-reset-state org-capture-after-finalize-hook))
  (mcg-org-capture-reset-state)
  (should-not mcg-org-capture--last-title)
  (should (equal "Untitled" (mcg-org-capture-last-title))))

(ert-deftest mcg-org-hugo-capture-state-is-reset-after-finalize ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let ((mcg-org-hugo-enable t)
        (org-capture-templates nil)
        (org-capture-after-finalize-hook nil))
    (mcg-org-hugo-setup-capture-templates)
    (setq mcg-org-hugo-capture--state '(:title "Old post" :slug "old-post"))
    (should (fboundp 'mcg-org-hugo-capture-reset-state))
    (should (memq #'mcg-org-hugo-capture-reset-state org-capture-after-finalize-hook))
    (mcg-org-hugo-capture-reset-state)
    (should-not mcg-org-hugo-capture--state)))

(ert-deftest mcg-org-download-image-path-follows-org-file-directory ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-download))
  (let* ((root (make-temp-file "mcg-org-download" t))
         (org-file (expand-file-name "My Note.org" root))
         (image-file (mcg-org-download-image-file "img.png" org-file)))
    (should (equal (expand-file-name "My Note.assets/img.png" root)
                   image-file))
    (should (equal "My Note.assets/img.png"
                   (mcg-org-download-relative-image-file image-file org-file)))))

(ert-deftest mcg-org-download-requires-saved-org-buffer ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-download))
  (with-temp-buffer
    (setq buffer-file-name nil)
    (should-error (mcg-org-download-assets-directory) :type 'user-error)))

(ert-deftest mcg-org-download-screenshot-method-quotes-image-path ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-download))
  (let* ((root (make-temp-file "mcg-org-download" t))
         (image-file (expand-file-name "My Note.assets/img 1.png" root))
         (method (mcg-org-download-screenshot-method image-file)))
    (should (string-match-p
             (regexp-quote (shell-quote-argument image-file))
             method))))

(ert-deftest mcg-org-agenda-configuration-is-available-after-require ()
  (require 'init-org-agenda)
  (should (equal org-agenda-files mcg-org-agenda-files))
  (should (eq org-agenda-format-date #'mcg-org-agenda-format-date-aligned))
  (should (assoc "d" org-agenda-custom-commands))
  (should (assoc "w" org-agenda-custom-commands)))

(ert-deftest mcg-org-agenda-file-and-time-grid-options-are-configurable ()
  (require 'init-org-agenda)
  (dolist (setting '(mcg-org-agenda-use-time-grid
                     mcg-org-agenda-current-time-string
                     mcg-org-agenda-time-grid))
    (should (boundp setting)))
  (let ((mcg-org-agenda-use-time-grid nil)
        (mcg-org-agenda-current-time-string "NOW")
        (mcg-org-agenda-time-grid '((daily today)
                                    (800 1200 1800)
                                    "..."
                                    "-----")))
    (mcg-org-agenda-setup-files)
    (mcg-org-agenda-setup-time-grid)
    (should-not org-agenda-use-time-grid)
    (should (equal "NOW" org-agenda-current-time-string))
    (should (equal mcg-org-agenda-time-grid org-agenda-time-grid))))

(ert-deftest mcg-org-agenda-display-options-are-configurable ()
  (require 'init-org-agenda)
  (dolist (setting '(mcg-org-agenda-include-diary
                     mcg-org-agenda-prefix-format
                     mcg-org-agenda-scheduled-leaders
                     mcg-org-agenda-deadline-leaders
                     mcg-org-time-stamp-formats
                     mcg-org-cycle-separator-lines
                     mcg-org-agenda-insert-diary-extract-time
                     mcg-org-agenda-sticky
                     mcg-org-agenda-time-leading-zero
                     mcg-org-agenda-tags-column
                     mcg-org-deadline-warning-days))
    (should (boundp setting)))
  (let ((mcg-org-agenda-include-diary nil)
        (mcg-org-agenda-prefix-format '((agenda . " %s")))
        (mcg-org-agenda-scheduled-leaders '("S " "Late %d "))
        (mcg-org-agenda-deadline-leaders '("D " "Soon %d " "Over %d "))
        (mcg-org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
        (mcg-org-cycle-separator-lines 1)
        (mcg-org-agenda-insert-diary-extract-time nil)
        (mcg-org-agenda-sticky nil)
        (mcg-org-agenda-time-leading-zero nil)
        (mcg-org-agenda-tags-column -60)
        (mcg-org-deadline-warning-days 10))
    (mcg-org-agenda-setup-display)
    (should-not org-agenda-include-diary)
    (should (equal mcg-org-agenda-prefix-format org-agenda-prefix-format))
    (should (equal mcg-org-agenda-scheduled-leaders org-agenda-scheduled-leaders))
    (should (equal mcg-org-agenda-deadline-leaders org-agenda-deadline-leaders))
    (should (equal mcg-org-time-stamp-formats org-time-stamp-formats))
    (should (= mcg-org-cycle-separator-lines org-cycle-separator-lines))
    (should-not org-agenda-insert-diary-extract-time)
    (should-not org-agenda-sticky)
    (should-not org-agenda-time-leading-zero)
    (should (= -60 org-agenda-tags-column))
    (should (= 10 org-deadline-warning-days))))

(ert-deftest mcg-org-agenda-custom-commands-are-configurable ()
  (require 'init-org-agenda)
  (should (boundp 'mcg-org-agenda-custom-commands))
  (let ((mcg-org-agenda-custom-commands
         '(("x" "Custom agenda" agenda ""))))
    (mcg-org-agenda-setup-custom-views)
    (should (equal mcg-org-agenda-custom-commands org-agenda-custom-commands))))

(ert-deftest mcg-org-appear-is-enabled-from-org-mode-hook ()
  (let ((org-mode-hook nil))
    (cl-letf (((symbol-function 'mcg-require-extension)
               (lambda (&rest _args) t))
              ((symbol-function 'org-appear-mode)
               (lambda (&optional _arg) nil)))
      (require 'init-org-appear))
    (should (fboundp 'mcg-org-appear-enable))
    (should (memq #'mcg-org-appear-enable org-mode-hook))))

(ert-deftest mcg-org-babel-defaults-are-conservative-and-configurable ()
  (require 'init-org-babel)
  (should (boundp 'mcg-org-babel-confirm-evaluate))
  (should (boundp 'mcg-org-export-babel-evaluate))
  (should mcg-org-babel-confirm-evaluate)
  (should-not mcg-org-export-babel-evaluate)
  (should (eq org-confirm-babel-evaluate mcg-org-babel-confirm-evaluate))
  (should (eq org-export-babel-evaluate mcg-org-export-babel-evaluate)))

(ert-deftest mcg-org-babel-default-language-surface-is-conservative ()
  (let ((source (expand-file-name "config-org/modules/org/init-org-babel.org"
                                  mcg-emacs-dir))
        (example-source
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (require 'init-org-babel)
    (should (boundp 'mcg-org-babel-enable-jupyter))
    (should (boundp 'mcg-org-babel-jupyter-default-header-args))
    (should-not mcg-org-babel-enable-jupyter)
    (should (equal mcg-org-babel-load-languages
                   '((emacs-lisp . t)
                     (shell . t)
                     (python . t)
                     (js . t)
                     (css . t)
                     (org . t))))
    (dolist (language '(C dot plantuml sql))
      (should-not (assq language mcg-org-babel-load-languages)))
    (should (equal mcg-org-babel-src-block-languages
                   '("emacs-lisp" "python" "shell" "zig" "js" "css" "org")))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("Conservative default languages"
                         "Extended Babel languages"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents example-source)
      (dolist (snippet '("Extended Org Babel languages"
                         "Jupyter Babel integration"
                         "mcg-org-babel-enable-jupyter"
                         "mcg-org-babel-jupyter-default-header-args"
                         "(C . t)"
                         "(dot . t)"
                         "(plantuml . t)"
                         "(sql . t)"
                         "(setq mcg-org-babel-src-block-languages"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-org-babel-jupyter-support-is-optional ()
  (require 'init-org-babel)
  (should (boundp 'mcg-org-babel-enable-jupyter))
  (should (boundp 'mcg-org-babel-jupyter-default-header-args))
  (let ((mcg-org-babel-enable-jupyter nil)
        (mcg-org-babel-src-block-languages '("emacs-lisp" "python"))
        (org-babel-default-header-args:jupyter-python '((:old . "value")))
        captured-languages)
    (cl-letf (((symbol-function 'org-babel-do-load-languages)
               (lambda (_symbol languages)
                 (setq captured-languages languages)))
              ((symbol-function 'executable-find)
               (lambda (_command) "jupyter")))
      (mcg-org-babel-setup-defaults)
      (mcg-org-babel-setup-languages))
    (should-not (assoc 'jupyter captured-languages))
    (should-not (member "jupyter-python" mcg-org-babel-src-block-languages))
    (should (equal '((:old . "value"))
                   org-babel-default-header-args:jupyter-python)))
  (let ((mcg-org-babel-enable-jupyter t)
        (mcg-org-babel-jupyter-default-header-args
         '((:async . "yes")
           (:session . "py")
           (:kernel . "python3")))
        (mcg-org-babel-src-block-languages '("emacs-lisp" "python"))
        captured-languages)
    (cl-letf (((symbol-function 'org-babel-do-load-languages)
               (lambda (_symbol languages)
                 (setq captured-languages languages)))
              ((symbol-function 'executable-find)
               (lambda (_command) "jupyter")))
      (mcg-org-babel-setup-defaults)
      (mcg-org-babel-setup-languages))
    (should (equal '((jupyter . t)) (last captured-languages)))
    (should (assoc 'jupyter captured-languages))
    (should (member "jupyter-python" mcg-org-babel-src-block-languages))
    (should (equal '((:async . "yes")
                     (:session . "py")
                     (:kernel . "python3"))
                   org-babel-default-header-args:jupyter-python))))

(ert-deftest mcg-org-babel-load-languages-are-configurable ()
  (require 'init-org-babel)
  (should (boundp 'mcg-org-babel-load-languages))
  (let ((mcg-org-babel-load-languages '((emacs-lisp . t)))
        captured-symbol
        captured-languages)
    (cl-letf (((symbol-function 'org-babel-do-load-languages)
               (lambda (symbol languages)
                 (setq captured-symbol symbol
                       captured-languages languages))))
      (mcg-org-babel-setup-languages))
    (should (eq captured-symbol 'org-babel-load-languages))
    (should (equal '((emacs-lisp . t)) captured-languages))))

(ert-deftest mcg-org-babel-zig-support-is-optional ()
  (require 'init-org-babel)
  (should (boundp 'mcg-org-babel-enable-zig))
  (should (boundp 'mcg-org-babel-zig-default-header-args))
  (let ((mcg-org-babel-enable-zig nil)
        (org-babel-load-languages nil)
        (org-babel-default-header-args:zig '((:old . "value"))))
    (mcg-org-babel-setup-zig)
    (should-not (assoc 'zig org-babel-load-languages))
    (should (equal '((:old . "value")) org-babel-default-header-args:zig)))
  (let ((mcg-org-babel-enable-zig t)
        (mcg-org-babel-zig-default-header-args '((:results . "silent")))
        (org-babel-load-languages nil)
        (org-babel-default-header-args:zig nil))
    (mcg-org-babel-setup-zig)
    (should (assoc 'zig org-babel-load-languages))
    (should (equal '((:results . "silent"))
                   org-babel-default-header-args:zig))))

(ert-deftest mcg-org-babel-src-block-languages-are-configurable ()
  (require 'init-org-babel)
  (should (boundp 'mcg-org-babel-src-block-languages))
  (let ((mcg-org-babel-src-block-languages '("emacs-lisp" "python"))
        captured-candidates)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _args)
                 (setq captured-candidates candidates)
                 "python")))
      (with-temp-buffer
        (call-interactively #'mcg/org-insert-src-block)
        (should (equal '("emacs-lisp" "python") captured-candidates))
        (should (search-backward "#+begin_src python" nil t))))))

(ert-deftest mcg-org-todo-keywords-and-faces-are-configurable ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org))
  (should (boundp 'mcg-org-todo-keywords))
  (should (boundp 'mcg-org-todo-keyword-faces))
  (let ((mcg-org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "|"
                                           "DONE(d!)" "CANCEL(c@/!)")))
        (mcg-org-todo-keyword-faces '(("TODO" :foreground "#7c7c75" :weight bold)
                                      ("DOING" :foreground "#d79921" :weight bold)
                                      ("DONE" :foreground "#50a14f" :weight bold)
                                      ("CANCEL" :foreground "#999999" :weight bold))))
    (mcg--setup-org-core)
    (should (equal mcg-org-todo-keywords org-todo-keywords))
    (should (equal mcg-org-todo-keyword-faces org-todo-keyword-faces))))

(ert-deftest mcg-org-ellipsis-is-configurable-and-ascii-safe ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org))
  (should (boundp 'mcg-org-ellipsis))
  (should (stringp mcg-org-ellipsis))
  (should (cl-every (lambda (char) (< char 128)) mcg-org-ellipsis))
  (let ((mcg-org-ellipsis " [...]"))
    (mcg--setup-org-core)
    (should (equal " [...]" org-ellipsis))))

(ert-deftest mcg-org-modern-todo-faces-are-configurable ()
  (setq mcg-org-bullets-alist '(("◉" "○")
                                ((45 . "▸"))))
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) t))
            ((symbol-function 'global-org-modern-mode)
             (lambda (&optional _arg) nil)))
    (require 'init-org-modern)
    (should (boundp 'mcg-org-modern-todo-faces))
    (let ((mcg-org-enable-org-modern t)
          (mcg-org-modern-todo-faces '(("DOING" :inverse-video t))))
      (require 'init-org-modern)
      (mcg--setup-org-modern)
      (should (equal mcg-org-modern-todo-faces org-modern-todo-faces)))))

(ert-deftest mcg-org-legacy-bullet-package-is-removed ()
  (let ((legacy-package (concat "org-" "superstar")))
    (dolist (path (list (format "config-org/modules/org/init-%s.org" legacy-package)
                        (format "site-lisp/config/modules/org/init-%s.el" legacy-package)
                        (format "site-lisp/extensions/org/%s-mode" legacy-package)))
      (should-not (file-exists-p (expand-file-name path mcg-emacs-dir))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "../.gitmodules" mcg-emacs-dir))
      (should-not (search-forward legacy-package nil t)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "config-org/modules/org/init-org.org"
                                              mcg-emacs-dir))
      (should-not (search-forward legacy-package nil t)))))

(ert-deftest mcg-org-numbering-is-optional-and-disabled-by-default ()
  (let ((enabled nil)
        (org-mode-hook nil))
    (cl-letf (((symbol-function 'mcg-require-extension)
               (lambda (&rest _args) t))
              ((symbol-function 'org-numbering-mode)
               (lambda (&optional arg)
                 (setq enabled arg))))
      (require 'init-org-numbering))
    (should (boundp 'mcg-org-enable-org-numbering))
    (should-not mcg-org-enable-org-numbering)
    (should-not enabled)
    (should-not (memq #'mcg-org-numbering-enable org-mode-hook))))

(ert-deftest mcg-obsolete-hugo-modules-are-removed ()
  (dolist (path '("config-org/modules/org/init-easy-hugo.org"
                  "config-org/modules/org/init-ox-hugo.org"
                  "site-lisp/config/modules/org/init-easy-hugo.el"
                  "site-lisp/config/modules/org/init-ox-hugo.el"))
    (should-not (file-exists-p (expand-file-name path mcg-emacs-dir)))))

(ert-deftest mcg-org-hugo-file-template-uses-file-level-metadata ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil))
            ((symbol-function 'read-from-minibuffer)
             (lambda (prompt &rest _args)
               (if (string-match-p "Categories" prompt)
                   "Emacs"
                 "")))
            ((symbol-function 'read-string)
             (let ((answers '("Org Capture Blog" "org-capture-blog")))
               (lambda (_prompt &rest _args)
                 (pop answers)))))
    (require 'init-org-hugo)
    (let* ((mcg-org-hugo-capture--state nil)
           (template (mcg/org-hugo-new-subtree-post-capture-template)))
      (should (string-match-p "^#\\+title: Org Capture Blog" template))
      (should (string-match-p "^#\\+hugo_base_dir:" template))
      (should (string-match-p "^#\\+hugo_section:" template))
      (should (string-match-p "^#\\+hugo_categories: Emacs" template))
      (should (string-match-p "^\\* TODO Draft" template)))))

(ert-deftest mcg-org-hugo-capture-keeps-post-title-when-slug-is-empty ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let* ((root (make-temp-file "mcg-hugo-blog" t))
         (mcg-blog-base-directory root)
         (mcg-org-blog-capture-directory root)
         (mcg-org-hugo-capture--state nil)
         (answers '("测试标题" ""))
         (metadata-answers '("" "技术随笔" "emacs org")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _args)
                 (pop answers)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (_prompt &rest _args)
                 (pop metadata-answers))))
      (let ((template (mcg/org-hugo-new-subtree-post-capture-template))
            (file (mcg-org-hugo-capture-post-file)))
        (should (string-match-p "^#\\+title: 测试标题" template))
        (should-not (string-match-p "undefined" template))
        (should (string-match-p "^#\\+hugo_categories: 技术随笔" template))
        (should (string-match-p "^#\\+hugo_tags: emacs org" template))
        (should (string-match-p "测试标题\\.org\\'" file))))))

(ert-deftest mcg-org-hugo-capture-uses-title-when-template-expands-before-target ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let* ((root (make-temp-file "mcg-hugo-blog" t))
         (mcg-blog-base-directory root)
         (mcg-org-blog-capture-directory root)
         (mcg-org-hugo-capture--state nil)
         (answers '("真实标题" ""))
         (metadata-answers '("" "技术随笔" "emacs org")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _args)
                 (pop answers)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (_prompt &rest _args)
                 (pop metadata-answers))))
      (let ((template (mcg/org-hugo-new-subtree-post-capture-template))
            (file (mcg-org-hugo-capture-post-file)))
        (should (string-match-p "^#\\+title: 真实标题" template))
        (should-not (string-match-p "^#\\+title: Untitled" template))
        (should (string-match-p "真实标题\\.org\\'" file))))))

(ert-deftest mcg-org-hugo-file-metadata-templates-use-plain-capture-type ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let ((mcg-org-hugo-enable t)
        (org-capture-templates nil))
    (mcg-org-hugo-setup-capture-templates)
    (dolist (key '("hb"))
      (should (eq (nth 2 (mcg-test-capture-template key)) 'plain)))))

(ert-deftest mcg-org-hugo-capture-directory-follows-hugo-base-and-postdir ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (let ((mcg-blog-base-directory "D:/workspaces/blog/mcge-blog")
        (mcg-org-hugo-postdir "config-org")
        (mcg-org-blog-capture-directory nil))
    (should (equal (file-name-as-directory
                    (expand-file-name "config-org" "D:/workspaces/blog/mcge-blog"))
                   (mcg-org-hugo-capture-directory)))))

(ert-deftest mcg-generated-config-example-preserves-utf-8-chinese ()
  (let ((generated-example
         (expand-file-name "site-lisp/config/private/config-example.el" mcg-emacs-dir)))
    (should (file-exists-p generated-example))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents generated-example))
      (should (search-forward "PostgreSQL 连接示例" nil t))
      (should-not (search-forward "杩炴帴绀轰緥" nil t)))))

(ert-deftest mcg-generated-private-configs-use-lexical-binding ()
  (dolist (relative-path '("site-lisp/config/private/config-example.el"
                           "site-lisp/config/private/config.el"))
    (let ((path (expand-file-name relative-path mcg-emacs-dir)))
      (when (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (should (search-forward "lexical-binding: t" (line-end-position) t)))))))

(ert-deftest mcg-doctor-lives-outside-core-bootstrap ()
  (let ((doctor-source
         (expand-file-name "config-org/core/core-doctor.org" mcg-emacs-dir))
        (bootstrap-source
         (expand-file-name "config-org/core/core-bootstrap.org" mcg-emacs-dir))
        (init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (should (file-exists-p doctor-source))
    (with-temp-buffer
      (insert-file-contents doctor-source)
      (dolist (snippet '(";;; core-doctor.el"
                         "(require 'core-bootstrap)"
                         "(defcustom mcg-doctor-dev-tool-checks"
                         "(defcustom mcg-doctor-treesit-language-checks"
                         "(defun mcg-doctor--check-dev-tools"
                         "(defun mcg-doctor--check-dev-environments"
                         "(defun mcg-doctor--python-environment-summary"
                         "(defun mcg-doctor--web-environment-summary"
                         "(defun mcg-doctor--zig-environment-summary"
                         "(defun mcg-doctor--check-treesit-grammars"
                         "(defun mcg-doctor "
                         "(provide 'core-doctor)"
                         "Python Development"
                         "\"uv\""
                         "\"poetry\""
                         "\"pdm\""
                         "Web Development"
                         "Zig Development"
                         "Writing Tools"
                         "External Dev Tools"
                         "Development Environments"
                         "Tree-sitter Grammars"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents bootstrap-source)
      (dolist (stale-snippet '("(defcustom mcg-doctor-dev-tool-checks"
                               "(defun mcg-doctor--check-dev-tools"
                               "(defun mcg-doctor "))
        (goto-char (point-min))
        (should-not (search-forward stale-snippet nil t))))
    (with-temp-buffer
      (insert-file-contents init-source)
      (should (search-forward "(require 'core-doctor)" nil t)))))

(ert-deftest mcg-doctor-dev-tool-checks-report-configured-tools ()
  (require 'core-doctor)
  (let ((mcg-doctor-dev-tool-checks
         '((:name "Test Stack"
            :tools ("always-present")
            :optional-tools ("optional-missing")))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (string= program "always-present")
                      (concat "/bin/" program)))))
      (let* ((result (mcg-doctor--check-dev-tools))
             (message (plist-get result :message)))
        (should (eq (plist-get result :status) :ok))
        (should (string-match-p "Test Stack" message))
        (should (string-match-p "always-present" message))
        (should (string-match-p "optional-missing" message))))))

(ert-deftest mcg-doctor-dev-environments-report-project-strategies ()
  (require 'core-doctor)
  (let ((root (make-temp-file "mcg-doctor-env" t)))
    (write-region "" nil (expand-file-name "uv.lock" root))
    (write-region "{}" nil (expand-file-name "package.json" root))
    (write-region "" nil (expand-file-name "eslint.config.js" root))
    (write-region "" nil (expand-file-name "build.zig" root))
    (let ((default-directory (file-name-as-directory root)))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (program)
                   (and (member program '("python" "node" "zig" "uv" "ruff"
                                          "pytest" "eslint" "zls"))
                        (concat "/bin/" program))))
                ((symbol-function 'mcg-python--project-tool)
                  (lambda (&optional _root) "uv"))
                ((symbol-function 'mcg-python--pytest-command)
                  (lambda (&optional _root) "uv run pytest"))
                ((symbol-function 'mcg-python-format-candidates)
                  (lambda (&optional _root)
                    '(("uv" "run" "ruff" "format" "--config"
                       "/tmp/ruff-format-single.toml"))))
                ((symbol-function 'mcg-format--web-project-has-antfu-eslint-p)
                 (lambda (&optional _root) t))
                ((symbol-function 'mcg-format--web-project-has-eslint-p)
                 (lambda (&optional _root) t))
                ((symbol-function 'mcg-format--web-project-has-prettier-p)
                 (lambda (&optional _root) nil))
                ((symbol-function 'mcg-format--web-external-candidates)
                 (lambda () '(("pnpm" "exec" "eslint" "--fix"))))
                ((symbol-function 'mcg-zig-build-command)
                 (lambda (action)
                   (pcase action
                     ('build "zig build")
                     ('run "zig build run")
                     ('test "zig build test")))))
        (let* ((result (mcg-doctor--check-dev-environments))
               (message (plist-get result :message)))
          (should (eq (plist-get result :status) :ok))
          (dolist (snippet '("Python Development"
                             "project tool: uv"
                             "pytest: uv run pytest"
                             "formatter: uv run ruff format --config /tmp/ruff-format-single.toml"
                             "Web Development"
                             "Antfu ESLint: yes"
                             "formatter: pnpm exec eslint --fix"
                             "Biome default: opt-in"
                             "Zig Development"
                             "build.zig: present"
                             "test: zig build test"
                             "run: zig build run"))
            (should (string-match-p (regexp-quote snippet) message))))))))

(ert-deftest mcg-doctor-renders-readable-ascii-report ()
  (require 'core-doctor)
  (cl-letf (((symbol-function 'display-buffer) #'ignore)
            ((symbol-function 'mcg-doctor--check-emacs-version)
             (lambda () (list :status :ok
                              :message "Emacs ready\nNative JSON ready")))
            ((symbol-function 'mcg-doctor--check-paths)
             (lambda () (list :status :warning
                              :message "Path warning\nCheck mcg-private-dir"
                              :action "Create the missing private directory.")))
            ((symbol-function 'mcg-doctor--check-core-modules)
             (lambda () (list :status :error
                              :message "core-lib missing"
                              :action "Rebuild generated config.")))
            ((symbol-function 'mcg-doctor--check-module-errors)
             (lambda () (list :status :ok :message "No module load errors")))
            ((symbol-function 'mcg-doctor--check-extensions)
             (lambda () (list :status :ok :message "Extensions ready")))
            ((symbol-function 'mcg-doctor--check-dev-tools)
             (lambda () (list :status :ok :message "Tools ready")))
            ((symbol-function 'mcg-doctor--check-dev-environments)
             (lambda () (list :status :ok :message "Strategies ready")))
            ((symbol-function 'mcg-doctor--check-treesit-grammars)
             (lambda () (list :status :ok :message "Grammars ready")))
            ((symbol-function 'mcg-doctor--check-gc-settings)
             (lambda () (list :status :ok :message "GC ready")))
            ((symbol-function 'mcg-doctor--check-native-comp)
             (lambda () (list :status :ok :message "Native comp ready"))))
    (mcg-doctor)
    (with-current-buffer "*MCG Doctor*"
      (let ((report (buffer-string)))
        (dolist (snippet '("MCG Emacs Doctor"
                           "[OK] Emacs Version"
                           "  Emacs ready"
                           "  Native JSON ready"
                           "[WARN] Path Configuration"
                           "  Path warning"
                           "  Check mcg-private-dir"
                           "  Action: Create the missing private directory."
                           "[ERROR] Core Modules"
                           "  core-lib missing"
                           "  Action: Rebuild generated config."
                           "Summary: 8 OK, 1 Warnings, 1 Errors"
                           "Configuration has errors that need attention."))
          (should (string-match-p (regexp-quote snippet) report)))
        (dolist (old-snippet '("[✓]" "[⚠]" "[✗]" "╔" "╚"))
          (should-not (string-match-p (regexp-quote old-snippet) report)))))))

(ert-deftest mcg-doctor-groups-results-by-severity ()
  (require 'core-doctor)
  (cl-letf (((symbol-function 'display-buffer) #'ignore)
            ((symbol-function 'mcg-doctor--check-emacs-version)
             (lambda () (list :status :ok :message "Emacs ready")))
            ((symbol-function 'mcg-doctor--check-paths)
             (lambda () (list :status :warning
                              :message "Path warning"
                              :action "Fix paths.")))
            ((symbol-function 'mcg-doctor--check-core-modules)
             (lambda () (list :status :error
                              :message "Core error"
                              :action "Rebuild core.")))
            ((symbol-function 'mcg-doctor--check-module-errors)
             (lambda () (list :status :ok :message "Modules ready")))
            ((symbol-function 'mcg-doctor--check-extensions)
             (lambda () (list :status :ok :message "Extensions ready")))
            ((symbol-function 'mcg-doctor--check-dev-tools)
             (lambda () (list :status :ok :message "Tools ready")))
            ((symbol-function 'mcg-doctor--check-dev-environments)
             (lambda () (list :status :ok :message "Strategies ready")))
            ((symbol-function 'mcg-doctor--check-treesit-grammars)
             (lambda () (list :status :ok :message "Grammars ready")))
            ((symbol-function 'mcg-doctor--check-gc-settings)
             (lambda () (list :status :ok :message "GC ready")))
            ((symbol-function 'mcg-doctor--check-native-comp)
             (lambda () (list :status :ok :message "Native comp ready"))))
    (mcg-doctor)
    (with-current-buffer "*MCG Doctor*"
      (let* ((report (buffer-string))
             (errors-pos (string-match-p "^Errors\n" report))
             (warnings-pos (string-match-p "^Warnings\n" report))
             (ok-pos (string-match-p "^OK\n" report))
             (error-check-pos (string-match-p "\\[ERROR\\] Core Modules" report))
             (warning-check-pos (string-match-p "\\[WARN\\] Path Configuration" report))
             (ok-check-pos (string-match-p "\\[OK\\] Emacs Version" report)))
        (should errors-pos)
        (should warnings-pos)
        (should ok-pos)
        (should (< errors-pos warnings-pos))
        (should (< warnings-pos ok-pos))
        (should (< errors-pos error-check-pos warnings-pos))
        (should (< warnings-pos warning-check-pos ok-pos))
        (should (< ok-pos ok-check-pos))
        (should (string-match-p "Summary: 8 OK, 1 Warnings, 1 Errors" report))))))

(ert-deftest mcg-doctor-warning-results-include-actions ()
  (require 'core-doctor)
  (let ((mcg-doctor-dev-tool-checks
         '((:name "Required Stack"
            :tools ("missing-required")
            :optional-tools ("missing-optional")))))
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
      (let ((result (mcg-doctor--check-dev-tools)))
        (should (eq (plist-get result :status) :warning))
        (should (string-match-p
                 "Install the missing required tools"
                 (plist-get result :action)))))
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil))
              ((symbol-function 'mcg-doctor--project-root)
               (lambda () default-directory)))
      (let ((result (mcg-doctor--check-dev-environments)))
        (should (eq (plist-get result :status) :warning))
        (should (string-match-p
                 "Install missing environment tools"
                 (plist-get result :action)))))
    (let ((mcg-doctor-treesit-language-checks '(python zig)))
      (let ((original-fboundp (symbol-function 'fboundp)))
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (symbol)
                     (and (not (eq symbol 'treesit-language-available-p))
                          (funcall original-fboundp symbol)))))
          (let ((result (mcg-doctor--check-treesit-grammars)))
            (should (eq (plist-get result :status) :warning))
            (should (string-match-p
                     "Use an Emacs build with tree-sitter support"
                     (plist-get result :action)))))))))

(ert-deftest mcg-doctor-treesit-checks-handle-missing-api ()
  (require 'core-doctor)
  (let ((mcg-doctor-treesit-language-checks '(markdown zig jsdoc)))
    (let ((original-fboundp (symbol-function 'fboundp)))
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (symbol)
                   (and (not (eq symbol 'treesit-language-available-p))
                        (funcall original-fboundp symbol)))))
        (let ((result (mcg-doctor--check-treesit-grammars)))
          (should (eq (plist-get result :status) :warning))
          (should (string-match-p "tree-sitter API not available"
                                  (plist-get result :message))))))))

(ert-deftest mcg-early-init-frame-colors-avoid-white-flash ()
  (let ((early-init-source
         (expand-file-name "config-org/early-init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents early-init-source)
      (dolist (setting '("mcg-initial-frame-background"
                         "mcg-initial-frame-foreground"
                         "(background-color . ,mcg-initial-frame-background)"
                         "(foreground-color . ,mcg-initial-frame-foreground)"))
        (goto-char (point-min))
        (should (search-forward setting nil t)))
      (goto-char (point-min))
      (should-not (search-forward "inhibit-redisplay t" nil t)))))

(ert-deftest mcg-early-init-startup-redisplay-is-released-once ()
  (let ((early-init-source
         (expand-file-name "config-org/early-init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents early-init-source)
      (dolist (setting '("mcg--startup-redisplay-released"
                         "mcg-release-startup-redisplay"
                         "unless mcg--startup-redisplay-released"
                         "add-hook 'window-setup-hook #'mcg-release-startup-redisplay"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-early-init-redirects-eln-cache-before-native-comp-settings ()
  (let ((early-init-source
         (expand-file-name "config-org/early-init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents early-init-source)
      (let ((redirect-position (progn
                                 (goto-char (point-min))
                                 (search-forward "startup-redirect-eln-cache" nil t)))
            (native-settings-position (progn
                                        (goto-char (point-min))
                                        (search-forward "native-comp-async-report-warnings-errors" nil t))))
        (should redirect-position)
        (should native-settings-position)
        (should (< redirect-position native-settings-position))))))

(ert-deftest mcg-generated-early-init-redirects-eln-cache-into-config-cache ()
  (skip-unless (boundp 'native-comp-eln-load-path))
  (let* ((early-init-file (expand-file-name "site-lisp/config/early-init.el" mcg-emacs-dir))
         (expected-eln-cache (expand-file-name ".cache/eln-cache/" mcg-emacs-dir))
         (original-eln-load-path (symbol-value 'native-comp-eln-load-path))
         (original-user-init-file user-init-file)
         (original-user-emacs-directory user-emacs-directory)
         (original-file-name-handler-alist file-name-handler-alist)
         (original-gc-cons-threshold gc-cons-threshold)
         (original-gc-cons-percentage gc-cons-percentage)
         (original-default-frame-alist default-frame-alist)
         (original-initial-frame-alist initial-frame-alist)
         (original-inhibit-redisplay inhibit-redisplay)
         (original-inhibit-message inhibit-message))
    (unwind-protect
        (progn
          (setq user-init-file (expand-file-name "init.el" mcg-emacs-dir))
          (setq user-emacs-directory mcg-emacs-dir)
          (set 'native-comp-eln-load-path
               (list (expand-file-name ".emacs.d/eln-cache/" temporary-file-directory)
                     (expand-file-name "system-native-lisp/" temporary-file-directory)))
          (load early-init-file nil t)
          (should (equal expected-eln-cache
                         (car (symbol-value 'native-comp-eln-load-path)))))
      (set 'native-comp-eln-load-path original-eln-load-path)
      (setq user-init-file original-user-init-file)
      (setq user-emacs-directory original-user-emacs-directory)
      (setq file-name-handler-alist original-file-name-handler-alist)
      (setq gc-cons-threshold original-gc-cons-threshold)
      (setq gc-cons-percentage original-gc-cons-percentage)
      (setq default-frame-alist original-default-frame-alist)
      (setq initial-frame-alist original-initial-frame-alist)
      (setq inhibit-redisplay original-inhibit-redisplay)
      (setq inhibit-message original-inhibit-message))))

(ert-deftest mcg-startup-heavy-modules-are-idle-by-default ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (dolist (setting '("(magit :load idle)"
                         "(dirvish :load idle)"
                         "(clutch :load idle)"
                         "(holo-layer :load idle)"
                         "(awesome-tray :load idle)"
                         "(org :load idle)"
                         "(vundo :load idle)"
                         "(yasnippet :load idle)"
                         "(compile :load idle)"
                         "(blink-search :load idle)"))
        (goto-char (point-min))
        (should (search-forward setting nil t)))
      (goto-char (point-min))
      (should (search-forward ":org" nil t)))))

(ert-deftest mcg-startup-exposes-high-value-idle-lazy-command-entries ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (dolist (snippet '(":tools 'blink-search \"tools/blink-search\""
                         "(\"C-c s b\" . blink-search)"
                         "(\"C-c s d\" . mcg/blink-search-grep-current-dir)"
                         "(\"C-c s p\" . mcg/blink-search-project-root)"
                         ":tools 'color-rg \"tools/color-rg\""
                         "(\"C-c s g\" . color-rg-search-input)"
                         "(\"C-c s G\" . mcg/color-rg-search-project)"
                         "(\"C-c s s\" . color-rg-search-symbol)"
                         "(\"C-c s S\" . mcg/color-rg-search-symbol-in-project)"
                         ":editor 'dirvish \"editor/dirvish\""
                         "(\"C-c d\" . dirvish)"
                         ":tools 'clutch \"tools/clutch\""
                         "(\"C-c ; c\" . clutch-query-console)"
                         "(\"C-c ; ?\" . clutch-dispatch)"
                         ":tools 'majutsu \"tools/majutsu\""
                         "(\"C-x M-j\" . majutsu)"
                         ":org 'org \"org/capture\""
                         "(\"C-c n c\" . org-capture)"
                         "(\"C-c n a\" . org-agenda)"
                         "(\"C-c n l\" . org-store-link)"
                         "(\"C-c n t\" . org-todo-list)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t)))
      (goto-char (point-min))
      (should (search-forward "(mcg--bind-idle-module-command-entries\n   :tools 'clutch \"tools/clutch\"" nil t))
      (goto-char (point-min))
      (should (search-forward "(mcg--bind-idle-module-command-entries\n   :tools 'majutsu \"tools/majutsu\"" nil t))
      (goto-char (point-min))
      (should (search-forward "(mcg--bind-idle-module-command-entries\n   :org 'org \"org/capture\"" nil t)))))

(ert-deftest mcg-dashboard-module-is-disabled-by-default-after-nerd-icons ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (goto-char (point-min))
      (should (search-forward "nerd-icons" nil t))
      (let ((nerd-icons-position (point)))
        (should (search-forward "-dashboard" nil t))
        (should (> (point) nerd-icons-position))))))

(ert-deftest mcg-dashboard-defaults-prefer-nerd-icons ()
  (let ((dashboard-source
         (expand-file-name "config-org/modules/ui/init-dashboard.org" mcg-emacs-dir)))
    (should (file-exists-p dashboard-source))
    (with-temp-buffer
      (insert-file-contents dashboard-source)
      (dolist (setting '("mcg-dashboard-icon-type"
                         "'nerd-icons"
                         "mcg-dashboard-startup-banner"
                         "'ascii"
                         "mcg-dashboard-banner-ascii"
                         "_      ____"
                         "\\____"
                         "mcg-dashboard-banner-title"
                         "nil"
                         "mcg-dashboard-footer-messages"
                         "By mcge"
                         "dashboard-footer-messages"
                         "dashboard-startup-banner"
                         "dashboard-banner-ascii"
                         "dashboard-icon-type"
                         "dashboard-set-heading-icons"
                         "dashboard-set-file-icons"
                         "mcg--setup-dashboard"))
        (goto-char (point-min))
        (should (search-forward setting nil t)))
      (goto-char (point-min))
      (should-not (search-forward "(require 'all-the-icons" nil t)))))

(ert-deftest mcg-dashboard-setup-configures-nerd-icons-after-dashboard-loads ()
  (let ((dashboard-setup-called nil)
        (dashboard-startup-render-called nil))
    (when (featurep 'init-dashboard)
      (unload-feature 'init-dashboard t))
    (provide 'nerd-icons)
    (setq dashboard-heading-icons nil
          dashboard-footer-icon nil
          dashboard-footer-messages nil
          dashboard-icon-type nil
          dashboard-set-heading-icons nil
          dashboard-set-file-icons nil)
    (cl-letf (((symbol-function 'mcg-require-extension)
               (lambda (&rest _args) t))
              ((symbol-function 'dashboard-setup-startup-hook)
               (lambda () (setq dashboard-setup-called t)))
              ((symbol-function 'dashboard-insert-startupify-lists)
               (lambda (&rest _args)
                 (setq dashboard-startup-render-called t)
                 (get-buffer-create "*dashboard*")))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (get-buffer-create buffer-or-name))))
      (require 'init-dashboard)
      (should-not dashboard-setup-called)
      (should dashboard-startup-render-called)
      (should (eq dashboard-icon-type 'nerd-icons))
      (should (eq dashboard-startup-banner 'ascii))
      (should (string-match-p "_      ____" dashboard-banner-ascii))
      (should (string-match-p "\\\\____" dashboard-banner-ascii))
      (should-not dashboard-banner-logo-title)
      (should (equal dashboard-footer-messages '("By mcge")))
      (should dashboard-set-heading-icons)
      (should dashboard-set-file-icons)
      (should (equal "nf-oct-history" (cdr (assq 'recents dashboard-heading-icons))))
      (should (equal "nf-oct-rocket" (cdr (assq 'projects dashboard-heading-icons)))))))

(ert-deftest mcg-dashboard-startup-displays-without-after-init-prerender ()
  (let ((dashboard-source
         (expand-file-name "config-org/modules/ui/init-dashboard.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents dashboard-source)
      (dolist (setting '("mcg-dashboard-open-startup-buffer"
                         "mcg-release-startup-redisplay"))
        (goto-char (point-min))
        (should (search-forward setting nil t)))
      (goto-char (point-min))
      (should-not (search-forward "(dashboard-setup-startup-hook)" nil t))
      (goto-char (point-min))
      (should-not (search-forward "initial-buffer-choice" nil t)))))

(ert-deftest mcg-dashboard-configures-recentf-before-startup-render ()
  (let ((dashboard-source
         (expand-file-name "config-org/modules/ui/init-dashboard.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents dashboard-source)
      (let ((recentf-position (progn
                                (goto-char (point-min))
                                (search-forward "mcg-dashboard-setup-recentf" nil t)))
            (render-position (progn
                               (goto-char (point-min))
                               (search-forward "mcg-dashboard-open-startup-buffer" nil t))))
        (should recentf-position)
        (should render-position)
        (should (< recentf-position render-position)))
      (dolist (setting '("recentf-save-file"
                         "recentf-auto-cleanup"
                         "'never"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-dashboard-early-display-does-not-refresh-twice-at-startup ()
  (let ((dashboard-source
         (expand-file-name "config-org/modules/ui/init-dashboard.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents dashboard-source)
      (goto-char (point-min))
      (should-not
       (search-forward "add-hook 'emacs-startup-hook #'dashboard-initialize" nil t))
      (goto-char (point-min))
      (should-not
       (search-forward "add-hook 'window-setup-hook #'dashboard-resize-on-hook" nil t))
      (goto-char (point-min))
      (should (search-forward "mcg-dashboard-enable-resize-refresh" nil t)))))

(ert-deftest mcg-dashboard-startup-renders-before-switching-once ()
  (let ((events nil)
        (initial-buffer-choice nil)
        (window-size-change-functions nil)
        (window-setup-hook nil)
        (dashboard-buffer-name "*dashboard*"))
    (when (featurep 'init-dashboard)
      (unload-feature 'init-dashboard t))
    (provide 'nerd-icons)
    (cl-letf (((symbol-function 'mcg-require-extension)
               (lambda (&rest _args) t))
              ((symbol-function 'dashboard-setup-startup-hook)
               (lambda ()
                 (ert-fail "dashboard-setup-startup-hook should not be used")))
              ((symbol-function 'dashboard-open)
               (lambda (&rest _args)
                 (ert-fail "dashboard-open switches before rendering and should not be used at startup")))
              ((symbol-function 'dashboard-insert-startupify-lists)
               (lambda (&rest _args)
                 (push 'render events)
                 (get-buffer-create dashboard-buffer-name)))
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (push (list 'switch buffer-or-name) events)
                 (get-buffer-create buffer-or-name))))
      (require 'init-dashboard)
      (should (equal (reverse events)
                     (list 'render (list 'switch dashboard-buffer-name))))
      (should-not initial-buffer-choice)
      (should (memq #'mcg-dashboard-enable-resize-refresh window-setup-hook)))))

(ert-deftest mcg-consult-integrates-completion-in-region-and-minibuffer-history ()
  (require 'init-consult)
  (should (eq (lookup-key global-map (kbd "C-x b")) #'consult-buffer))
  (should (eq (lookup-key global-map (kbd "C-x 4 b")) #'consult-buffer-other-window))
  (should (eq (lookup-key global-map (kbd "C-x 5 b")) #'consult-buffer-other-frame))
  (should (eq completion-in-region-function #'consult-completion-in-region))
  (should (eq (lookup-key minibuffer-local-map (kbd "C-r")) #'consult-history))
  (should (eq xref-show-definitions-function #'consult-xref)))

(ert-deftest mcg-consult-owns-explicit-c-c-s-search-prefix ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(define-prefix-command 'mcg-search-map)"
                         "(mcg-bind-key \"C-c s\" 'mcg-search-map \"completion/consult\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t)))))
  (require 'init-consult)
  (should (keymapp (lookup-key global-map (kbd "C-c s")))))

(ert-deftest mcg-project-switch-buffer-prefers-consult-project-buffer ()
  (require 'init-project)
  (let ((called nil))
    (cl-letf (((symbol-function 'consult-project-buffer)
               (lambda ()
                 (interactive)
                 (setq called 'consult)))
              ((symbol-function 'project-switch-to-buffer)
               (lambda ()
                 (interactive)
                 (setq called 'project)))
              ((symbol-function 'switch-to-buffer)
               (lambda ()
                 (interactive)
                 (setq called 'switch))))
      (mcg/project-switch-buffer)
      (should (eq called 'consult)))))

(ert-deftest mcg-project-find-file-prefers-consult-find ()
  (require 'init-project)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-project--require-root)
               (lambda () "/tmp/project/"))
              ((symbol-function 'consult-find)
               (lambda (directory)
                 (interactive)
                 (setq called (cons 'consult directory))))
              ((symbol-function 'project-find-file)
               (lambda ()
                 (interactive)
                 (setq called 'project))))
      (mcg/project-find-file)
      (should (equal called '(consult . "/tmp/project/"))))))

(ert-deftest mcg-core-project-root-or-default-prefers-project-module-helper ()
  (require 'core-lib)
  (let ((fallback-directory "/tmp/fallback/"))
    (cl-letf (((symbol-function 'mcg-project-current-root)
               (lambda (&optional _maybe-prompt) "/tmp/mcg-project/"))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt) nil)))
      (let ((default-directory fallback-directory))
        (should (equal (mcg-project-root-or-default) "/tmp/mcg-project/"))))))

(ert-deftest mcg-core-project-root-or-default-falls-back-to-default-directory ()
  (require 'core-lib)
  (let ((fallback-directory "/tmp/fallback/"))
    (cl-letf (((symbol-function 'mcg-project-current-root)
               (lambda (&optional _maybe-prompt) nil))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt) nil)))
      (let ((default-directory fallback-directory))
        (should (equal (mcg-project-root-or-default) "/tmp/fallback/"))))))

(ert-deftest mcg-project-root-wrapper-logic-lives-in-core-lib ()
  (let ((cases '(("config-org/modules/tools/init-compile.org"
                  "(defun mcg-compile--project-root"
                  "(mcg-project-root-or-default)")
                 ("config-org/modules/tools/init-blink-search.org"
                  "(defun mcg-blink-search-project-root"
                  "(mcg-project-root-or-default)")
                 ("config-org/modules/tools/init-color-rg.org"
                  "(defun mcg-color-rg-project-root"
                  "(mcg-project-root-or-default)"))))
    (dolist (case cases)
      (let ((source (expand-file-name (nth 0 case) mcg-emacs-dir)))
        (with-temp-buffer
          (insert-file-contents source)
          (should-not (search-forward (nth 1 case) nil t))
          (goto-char (point-min))
          (should (search-forward (nth 2 case) nil t)))))))

(ert-deftest mcg-compile-errors-prefers-consult-compile-error ()
  (require 'init-compile)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-compile--consult-compile-error-p)
               (lambda () t))
              ((symbol-function 'consult-compile-error)
               (lambda ()
                 (interactive)
                 (setq called 'consult)))
              ((symbol-function 'next-error)
               (lambda ()
                 (interactive)
                 (setq called 'next-error))))
      (mcg-compile-errors)
      (should (eq called 'consult)))))

(ert-deftest mcg-compile-errors-falls-back-to-next-error ()
  (require 'init-compile)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-compile--consult-compile-error-p)
               (lambda () nil))
              ((symbol-function 'consult-compile-error)
               (lambda ()
                 (interactive)
                 (setq called 'consult)))
              ((symbol-function 'next-error)
               (lambda ()
                 (interactive)
                 (setq called 'next-error))))
      (mcg-compile-errors)
      (should (eq called 'next-error)))))

(ert-deftest mcg-compile-binds-m-g-e-to-compile-errors-wrapper ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-compile.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "(mcg-bind-key \"M-g e\" #'mcg-compile-errors \"tools/compile\")"
                              nil t)))))

(ert-deftest mcg-compile-python-test-reuses-python-tool-detection ()
  (require 'init-compile)
  (let ((root (make-temp-file "mcg-compile-python" t))
        (called nil))
    (write-region "[project]\nname = \"demo\"\n" nil
                  (expand-file-name "pyproject.toml" root))
    (cl-letf (((symbol-function 'mcg-python--pytest-command)
               (lambda (&optional command-root)
                 (setq called command-root)
                 "uv run pytest")))
      (should (equal (mcg-compile--guess-command root 'test)
                     "uv run pytest"))
      (should (equal called (file-name-as-directory root))))))

(ert-deftest mcg-compile-source-documents-python-command-delegation ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-compile.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defun mcg-compile--python-command"
                         "(fboundp 'mcg-python--pytest-command)"
                         "(mcg-python--pytest-command (file-name-as-directory root))"
                         "python -m pytest"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-compile-zig-commands-reuse-zig-build-command-helper ()
  (require 'init-compile)
  (let ((root (make-temp-file "mcg-compile-zig" t))
        (called nil))
    (write-region "pub fn build(_: *std.Build) void {}\n" nil
                  (expand-file-name "build.zig" root))
    (cl-letf (((symbol-function 'mcg-zig-build-command)
               (lambda (action)
                 (setq called action)
                 "zig build custom-test")))
      (should (equal (mcg-compile--guess-command root 'test)
                     "zig build custom-test"))
      (should (eq called 'test)))))

(ert-deftest mcg-compile-source-documents-zig-command-delegation ()
  (let ((compile-source
         (expand-file-name "config-org/modules/tools/init-compile.org" mcg-emacs-dir))
        (zig-source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents compile-source)
      (dolist (snippet '("(defun mcg-compile--zig-command"
                         "(fboundp 'mcg-zig-build-command)"
                         "(mcg-zig-build-command action)"
                         "zig build test"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents zig-source)
      (dolist (snippet '("(defun mcg-zig-build-command"
                         "(mcg-zig-build-command 'build)"
                         "(mcg-zig-build-command 'run)"
                         "(mcg-zig-build-command 'test)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-diagnostics-binds-m-g-f-to-diagnostics-list-wrapper ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-diagnostics.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "(mcg-bind-key \"M-g f\" #'mcg-diagnostics-list \"tools/diagnostics\")"
                              nil t)))))

(ert-deftest mcg-blink-search-org-title-matches-module ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-blink-search.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "#+TITLE: Blink Search Module" nil t))
      (should (search-forward "* Blink Search Module" nil t))
      (goto-char (point-min))
      (should-not (search-forward "Auto Save Module" nil t)))))

(ert-deftest mcg-search-extension-modules-describe-extending-c-c-s-prefix ()
  (dolist (case '(("config-org/modules/tools/init-blink-search.org"
                   "Extend the shared C-c s search prefix"
                   "Blink-search keybindings configured")
                  ("config-org/modules/tools/init-color-rg.org"
                   "Extend the shared C-c s search prefix"
                   "Color-rg keybindings configured")))
    (let ((source (expand-file-name (car case) mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents source)
        (dolist (snippet (cdr case))
          (goto-char (point-min))
          (should (search-forward snippet nil t)))))))

(ert-deftest mcg-blink-search-project-root-prefers-mcg-project-root-helper ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-blink-search))
  (let ((called-directory nil)
        (fallback-directory "/tmp/fallback/"))
    (cl-letf (((symbol-function 'mcg-project-current-root)
               (lambda (&optional _maybe-prompt) "/tmp/mcg-project/"))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt) nil))
              ((symbol-function 'blink-search)
               (lambda ()
                 (interactive)
                 (setq called-directory default-directory))))
      (let ((default-directory fallback-directory))
        (mcg/blink-search-project-root))
      (should (equal called-directory "/tmp/mcg-project/")))))

(ert-deftest mcg-blink-search-project-root-does-not-depend-on-projectile ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-blink-search.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should-not (search-forward "projectile-project-root" nil t)))))

(ert-deftest mcg-color-rg-project-keybindings-use-mcg-wrappers ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-color-rg.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-c s G\" 'mcg/color-rg-search-project \"tools/color-rg\")"
                         "(mcg-bind-key \"C-c s S\" 'mcg/color-rg-search-symbol-in-project \"tools/color-rg\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-color-rg-search-project-prefers-mcg-project-root-helper ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-color-rg))
  (let ((called-keyword nil)
        (called-directory nil)
        (fallback-directory "/tmp/fallback/"))
    (cl-letf (((symbol-function 'mcg-project-current-root)
               (lambda (&optional _maybe-prompt) "/tmp/mcg-project/"))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt) nil))
              ((symbol-function 'color-rg-read-input)
               (lambda () "needle"))
              ((symbol-function 'color-rg-search-input)
               (lambda (keyword directory &optional _globs _file-list)
                 (setq called-keyword keyword
                       called-directory directory))))
      (let ((default-directory fallback-directory))
        (mcg/color-rg-search-project))
      (should (equal called-keyword "needle"))
      (should (equal called-directory "/tmp/mcg-project/")))))

(ert-deftest mcg-color-rg-search-project-with-type-uses-mcg-project-root-helper ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-color-rg))
  (let ((called-directory nil)
        (called-globs nil))
    (cl-letf (((symbol-function 'mcg-project-current-root)
               (lambda (&optional _maybe-prompt) "/tmp/mcg-project/"))
              ((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt) nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) "*.el"))
              ((symbol-function 'color-rg-read-input)
               (lambda () "needle"))
              ((symbol-function 'color-rg-search-input)
               (lambda (_keyword directory &optional globs _file-list)
                 (setq called-directory directory
                       called-globs globs))))
      (mcg/color-rg-search-project-with-type)
      (should (equal called-directory "/tmp/mcg-project/"))
      (should (equal called-globs "*.el")))))

(ert-deftest mcg-embark-integrates-prefix-help-and-describe-bindings ()
  (require 'init-embark)
  (should (eq prefix-help-command #'embark-prefix-help-command))
  (should (eq (lookup-key global-map [remap describe-bindings]) #'embark-bindings)))

(ert-deftest mcg-embark-consult-enables-preview-in-collect-buffers ()
  (defvar embark-collect-mode-hook nil)
  (cl-letf (((symbol-value 'embark-collect-mode-hook) nil))
    (provide 'embark-consult)
    (require 'init-embark)
    (should (memq #'consult-preview-at-point-mode embark-collect-mode-hook))))

(ert-deftest mcg-ef-themes-exposes-consult-theme-switching ()
  (let ((source
         (expand-file-name "config-org/modules/ui/init-ef-themes.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defun mcg-ui-switch-theme ()"
                         "(if (fboundp 'consult-theme)"
                         "(call-interactively #'consult-theme)"
                         "(completing-read \"Theme: \""
                         "(mcg-bind-key \"C-c T\" #'mcg-ui-switch-theme \"ui/theme\")"
                         "(consult-customize consult-theme :preview-key '(:debounce 0.5 any))"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-pulsar-module-is-registered-as-ui ()
  (let ((source (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "pulsar" nil t)))))

(ert-deftest mcg-pulsar-enables-global-visual-location-feedback ()
  (require 'init-pulsar)
  (should (bound-and-true-p pulsar-global-mode))
  (should (eq pulsar-face 'pulsar-cyan))
  (dolist (fn '(recenter-top-bottom
                other-window
                next-error
                previous-error
                consult-line
                consult-ripgrep
                xref-find-definitions
                xref-go-back))
    (should (memq fn pulsar-pulse-functions))))

(ert-deftest mcg-nerd-icons-font-warning-is-suppressed-in-batch ()
  (require 'init-nerd-icons)
  (let ((captured-warnings nil)
        (noninteractive t))
    (cl-letf (((symbol-function 'mcg-require-extension)
              (lambda (&rest _args) t))
              ((symbol-function 'mcg-get-icon-font)
               (lambda () "MCG Definitely Missing Nerd Font"))
              ((symbol-function 'mcg--log-internal)
               (lambda (level format-string &rest args)
                 (when (eq level 'warning)
                   (push (apply #'format format-string args)
                         captured-warnings)))))
      (mcg--setup-nerd-icons))
    (should-not (member "nerd-icons font missing, run M-x nerd-icons-install-fonts"
                        captured-warnings))))

(ert-deftest mcg-help-remaps-standard-describe-commands-to-helpful-aware-wrappers ()
  (let ((source
         (expand-file-name "config-org/modules/enhance/init-help.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defun mcg-help-describe-command ()"
                         "helpful-command"
                         "(define-key global-map [remap describe-command] #'mcg-help-describe-command)"
                         "(define-key global-map [remap describe-function] #'mcg-help-describe-function)"
                         "(define-key global-map [remap describe-variable] #'mcg-help-describe-variable)"
                         "(define-key global-map [remap describe-key] #'mcg-help-describe-key)"
                         "(define-key global-map [remap describe-symbol] #'mcg-help-describe-symbol)"
                         "(with-eval-after-load 'helpful"
                         "(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)"
                         "(define-key helpful-mode-map (kbd \"C-x K\") #'helpful-kill-buffers)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-help-exposes-describe-keymap ()
  (let ((source
         (expand-file-name "config-org/modules/enhance/init-help.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-h K\" #'describe-keymap \"enhance/help\")"
                         "(mcg-bind-key \"C-c h K\" #'describe-keymap \"enhance/help\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-help-exposes-helpful-function ()
  (let ((source
         (expand-file-name "config-org/modules/enhance/init-help.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(with-eval-after-load 'helpful"
                         "(mcg-bind-key \"C-h F\" #'helpful-function \"enhance/help\")"
                         "(mcg-bind-key \"C-c h F\" #'helpful-function \"enhance/help\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-help-describe-at-point-prefers-helpful-when-available ()
  (require 'init-help)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-help--helpful-available-p)
               (lambda () t))
              ((symbol-function 'helpful-at-point)
               (lambda ()
                 (interactive)
                 (setq called 'helpful)))
              ((symbol-function 'describe-symbol)
               (lambda (&rest _args)
                 (setq called 'fallback))))
      (mcg-help-describe-at-point)
      (should (eq called 'helpful)))))

(ert-deftest mcg-help-describe-at-point-falls-back-to-describe-symbol ()
  (require 'init-help)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-help--helpful-available-p)
               (lambda () nil))
              ((symbol-function 'symbol-at-point)
               (lambda () 'alpha))
              ((symbol-function 'describe-symbol)
               (lambda (symbol)
                 (setq called symbol))))
      (mcg-help-describe-at-point)
      (should (eq called 'alpha)))))

(ert-deftest mcg-consult-adds-standard-m-g-navigation-bindings ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"M-g g\" #'consult-goto-line \"completion/consult\")"
                         "(mcg-bind-key \"M-g M-g\" #'consult-goto-line \"completion/consult\")"
                         "(mcg-bind-key \"M-g o\" #'consult-outline \"completion/consult\")"
                         "(mcg-bind-key \"M-g i\" #'consult-imenu \"completion/consult\")"
                         "(mcg-bind-key \"M-g I\" #'consult-imenu-multi \"completion/consult\")"
                         "(mcg-bind-key \"M-g m\" #'consult-mark \"completion/consult\")"
                         "(mcg-bind-key \"M-g k\" #'consult-global-mark \"completion/consult\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-consult-adds-standard-m-s-search-bindings ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"M-s f\" #'consult-find \"completion/consult\")"
                         "(mcg-bind-key \"M-s g\" #'consult-grep \"completion/consult\")"
                         "(mcg-bind-key \"M-s G\" #'consult-git-grep \"completion/consult\")"
                         "(mcg-bind-key \"M-s r\" #'consult-ripgrep \"completion/consult\")"
                         "(mcg-bind-key \"M-s l\" #'consult-line \"completion/consult\")"
                         "(mcg-bind-key \"M-s L\" #'consult-line-multi \"completion/consult\")"
                         "(mcg-bind-key \"M-s k\" #'consult-keep-lines \"completion/consult\")"
                         "(mcg-bind-key \"M-s u\" #'consult-focus-lines \"completion/consult\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-consult-binds-bookmarks-to-consult-bookmark ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-x r b\" #'consult-bookmark \"completion/consult\")"
                         "(define-key mcg-consult-buffer-map \"m\" #'consult-bookmark)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-consult-binds-recent-files-to-consult-recent-file ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-c f r\" #'consult-recent-file \"completion/consult\")"
                         "(define-key mcg-consult-buffer-map \"r\" #'consult-recent-file)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-consult-adds-standard-command-and-register-bindings ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(require 'consult-register nil t)"
                         "(mcg-bind-key \"C-x M-:\" #'consult-complex-command \"completion/consult\")"
                         "(mcg-bind-key \"M-#\" #'consult-register-load \"completion/consult\")"
                         "(mcg-bind-key \"M-'\" #'consult-register-store \"completion/consult\")"
                         "(mcg-bind-key \"C-M-#\" #'consult-register \"completion/consult\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-consult-binds-project-buffer-to-consult-project-buffer ()
  (let ((source
         (expand-file-name "config-org/modules/completion/init-consult.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-x p b\" #'consult-project-buffer \"completion/consult\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-project-binds-switch-buffer-to-mcg-project-switch-buffer ()
  (let ((source
         (expand-file-name "config-org/modules/tools/init-project.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-c p b\" #'mcg/project-switch-buffer \"tools/project\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-treesit-auto-owns-common-mode-associations ()
  (let ((treesit-source
         (expand-file-name "config-org/modules/editor/init-treesit.org" mcg-emacs-dir))
        (web-source
         (expand-file-name "config-org/modules/lang/init-web.org" mcg-emacs-dir))
        (zig-source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents treesit-source)
      (should (search-forward "(mcg-require-extension 'treesit-auto t)" nil t))
      (goto-char (point-min))
      (should (search-forward "(treesit-auto-add-to-auto-mode-alist 'all)" nil t))
      (goto-char (point-min))
      (should (search-forward "(global-treesit-auto-mode)" nil t)))
    (dolist (source (list web-source zig-source))
      (with-temp-buffer
        (insert-file-contents source)
        (should-not (search-forward "(add-to-list 'auto-mode-alist '(\"\\\\.css\\\\'\" . css-ts-mode))" nil t))
        (goto-char (point-min))
        (should-not (search-forward "mcg-setup-lang-mode" nil t))))))

(ert-deftest mcg-treesit-declares-jsdoc-grammar-source ()
  (let ((source
         (expand-file-name "config-org/modules/editor/init-treesit.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(jsdoc . (\"https://github.com/tree-sitter/tree-sitter-jsdoc\"))"
                         "jsdoc"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-python-module-is-enabled-and-trigger-backed ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir))
        (modules-source
         (expand-file-name "config-org/core/core-modules.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (should (search-forward ":lang" nil t))
      (should (search-forward "python" nil t)))
    (with-temp-buffer
      (insert-file-contents modules-source)
      (should (search-forward "((:lang . python) . (\"\\\\.py\\\\'\" . python-ts-mode))" nil t)))))

(ert-deftest mcg-python-module-provides-core-dev-commands ()
  (let ((source
         (expand-file-name "config-org/modules/lang/init-python.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
       (dolist (snippet '("defcustom mcg-python-test-command"
                          "defcustom mcg-python-ruff-command"
                          "defcustom mcg-python-use-ipython"
                          "defcustom mcg-python-ipython-args"
                          "defconst mcg-python-ruff-format-quote-style"
                          "defconst mcg-python-ruff-format-config-file-name"
                          "defun mcg-python-mode-setup"
                          "defun mcg/python-pytest-project"
                          "defun mcg/python-pytest-file"
                          "defun mcg/python-ruff-check"
                          "defun mcg/python-ruff-format"
                          "defun mcg-python--ruff-format-config-file"
                          "defun mcg-python--ruff-format-command"
                          "defun mcg-python--repl-command"
                          "defun mcg/python-repl"
                          "python-ts-mode-hook"
                          "python-mode-hook"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-python-module-detects-project-tooling ()
  (let ((source
         (expand-file-name "config-org/modules/lang/init-python.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
       (dolist (snippet '("defcustom mcg-python-detect-project-tools t"
                          "defun mcg-python--project-tool"
                          "\"uv.lock\""
                          "\"poetry.lock\""
                          "\"pdm.lock\""
                          "mcg-python-ruff-format-quote-style"
                          "mcg-python-ruff-format-config-file-name"
                          "defun mcg-python--pytest-command"
                          "defun mcg-python--ruff-command"
                          "defun mcg-python-format-candidates"
                         "executable-find"
                         "(mcg-python--pytest-command)"
                         "(mcg-python--ruff-command \"check\" \".\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-python-project-tool-commands-choose-available-runner ()
  (load (expand-file-name "site-lisp/config/modules/lang/init-python.el"
                          mcg-emacs-dir)
        nil t)
  (let ((uv-root (make-temp-file "mcg-python-uv" t))
        (poetry-root (make-temp-file "mcg-python-poetry" t))
        (pdm-root (make-temp-file "mcg-python-pdm" t))
        (plain-root (make-temp-file "mcg-python-plain" t)))
    (write-region "" nil (expand-file-name "uv.lock" uv-root))
    (write-region "" nil (expand-file-name "poetry.lock" poetry-root))
    (write-region "" nil (expand-file-name "pdm.lock" pdm-root))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (tool)
                 (and (member tool '("uv" "poetry" "pdm" "ruff" "python"))
                      tool))))
      (should (equal (mcg-python--pytest-command uv-root) "uv run pytest"))
      (should (equal (mcg-python--pytest-command poetry-root) "poetry run pytest"))
      (should (equal (mcg-python--pytest-command pdm-root) "pdm run pytest"))
      (should (equal (mcg-python--pytest-command plain-root) "python -m pytest"))
      (cl-letf (((symbol-function 'mcg-python--ruff-format-config-file)
                 (lambda () "/tmp/ruff-format-single.toml")))
        (should (equal (mcg-python-format-candidates uv-root)
                       '(("uv" "run" "ruff" "format" "--config"
                          "/tmp/ruff-format-single.toml")
                         ("ruff" "format" "--config"
                          "/tmp/ruff-format-single.toml")
                         ("black" "--skip-string-normalization")))))
      (let ((default-directory uv-root))
        (should (equal (mcg-python--ruff-command "check" ".")
                       "uv run ruff check ."))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (tool)
                 (and (member tool '("ruff" "python"))
                      tool))))
      (should (equal (mcg-python--pytest-command uv-root) "python -m pytest"))
      (let ((default-directory uv-root))
        (should (equal (mcg-python--ruff-command "check" ".")
                       "ruff check ."))))))

(ert-deftest mcg-python-repl-prefers-ipython-and-shows-buffer ()
  (load (expand-file-name "site-lisp/config/modules/lang/init-python.el"
                          mcg-emacs-dir)
        nil t)
  (let (command dedicated show)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (tool)
                 (pcase tool
                   ("ipython" "C:/Python/Scripts/ipython.exe")
                   ("python" "C:/Python/python.exe"))))
              ((symbol-function 'run-python)
               (lambda (cmd dedicated-arg show-arg)
                 (setq command cmd
                       dedicated dedicated-arg
                       show show-arg)
                 'python-process)))
      (should (eq (mcg/python-repl) 'python-process))
      (should (string-match-p "ipython" command))
      (should (string-match-p "--simple-prompt" command))
      (should-not dedicated)
      (should show))))

(ert-deftest mcg-python-repl-falls-back-to-python-when-ipython-is-missing ()
  (load (expand-file-name "site-lisp/config/modules/lang/init-python.el"
                          mcg-emacs-dir)
        nil t)
  (cl-letf (((symbol-function 'executable-find)
             (lambda (tool)
               (and (equal tool "python")
                    "C:/Python/python.exe"))))
    (let ((command (mcg-python--repl-command)))
      (should (string-match-p "python" command))
      (should-not (string-match-p "--simple-prompt" command)))))

(ert-deftest mcg-python-ruff-format-config-file-contains-single-quote-toml ()
  (load (expand-file-name "site-lisp/config/modules/lang/init-python.el"
                          mcg-emacs-dir)
        nil t)
  (let ((mcg-cache-dir (make-temp-file "mcg-ruff-format" t)))
    (let ((file (mcg-python--ruff-format-config-file)))
      (should (string-suffix-p "/ruff-format-single.toml"
                               (replace-regexp-in-string "\\\\" "/" file)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string)
                       "[format]\nquote-style = \"single\"\n"))))))

(ert-deftest mcg-python-ruff-format-prefers-project-config-when-present ()
  (load (expand-file-name "site-lisp/config/modules/lang/init-python.el"
                          mcg-emacs-dir)
        nil t)
  (let ((root (make-temp-file "mcg-ruff-project" t)))
    (write-region "[tool.ruff]\nline-length = 88\n" nil
                  (expand-file-name "pyproject.toml" root))
    (cl-letf (((symbol-function 'mcg-python--ruff-format-config-file)
               (lambda ()
                 (error "project config should win"))))
      (should (mcg-python--ruff-project-config-p root))
      (should (equal (mcg-python--ruff-format-args root) '("format")))
      (should (equal (mcg-python-format-candidates root)
                     '(("ruff" "format")
                       ("black" "--skip-string-normalization")))))))

(ert-deftest mcg-lsp-bridge-ty-completions-insert-function-parentheses ()
  (let ((source (expand-file-name
                 "site-lisp/extensions/lsp/lsp-bridge/langserver/ty.json"
                 mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "\"completeFunctionParentheses\": true" nil t)))))

(ert-deftest mcg-format-supports-python-formatters ()
  (let ((source
         (expand-file-name "config-org/modules/editor/init-format.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("python-mode"
                         "python-ts-mode"
                         "(python-mode . mcg-format-with-external-command)"
                         "(python-ts-mode . mcg-format-with-external-command)"
                         "(python-mode . mcg-python-format-candidates)"
                         "(python-ts-mode . mcg-python-format-candidates)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-format-on-save-skips-unmodified-buffers ()
  (require 'init-format)
  (let ((mcg-format-on-save t)
        (calls 0))
    (with-temp-buffer
      (setq major-mode 'json-ts-mode)
      (insert "{\"name\":\"demo\"}\n")
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'mcg-format-buffer)
                 (lambda ()
                   (setq calls (1+ calls)))))
        (mcg-format-buffer-maybe))
      (should (= calls 0)))))

(ert-deftest mcg-web-deferred-trigger-covers-common-web-files ()
  (let ((modules-source
         (expand-file-name "config-org/core/core-modules.org" mcg-emacs-dir))
        (web-source
         (expand-file-name "config-org/modules/lang/init-web.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents modules-source)
      (dolist (snippet '("(:lang . web)"
                         "html?"
                         "css"
                         "jsx?"
                         "tsx?"
                         "vue"
                         "mcg-web-deferred-mode"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents web-source)
      (dolist (snippet '("defun mcg-web-deferred-mode"
                         "web-mode"
                         "css-ts-mode"
                         "js-ts-mode"
                         "typescript-ts-mode"
                         "tsx-ts-mode"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-writing-modules-are-enabled-with-safe-loading ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir))
        (modules-source
         (expand-file-name "config-org/core/core-modules.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (dolist (snippet '(":writing"
                         "markdown"
                         "vale"
                         "(markdown :load deferred)"
                         "(vale :load idle)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents modules-source)
      (should (search-forward "((:writing . markdown) . (\"\\\\.md\\\\'\" . mcg-markdown-deferred-mode))"
                              nil t))
      (should-not (search-forward "((:writing . vale)" nil t)))))

(ert-deftest mcg-markdown-module-provides-safe-mode-dispatch ()
  (let ((source
         (expand-file-name "config-org/modules/writing/init-markdown.org" mcg-emacs-dir)))
    (should (file-exists-p source))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defcustom mcg-markdown-prefer-treesit t"
                         "(defun mcg-markdown-deferred-mode"
                         "markdown-ts-mode"
                         "treesit-language-available-p"
                         "markdown-mode"
                         "visual-line-mode"
                         "mcg/markdown-preview"
                         "(define-key map (kbd \"C-c C-p\") #'mcg/markdown-preview"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-vale-module-is-optional-and-hook-based ()
  (let ((source
         (expand-file-name "config-org/modules/writing/init-vale.org" mcg-emacs-dir))
        (example-source
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (should (file-exists-p source))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defcustom mcg-vale-enable nil"
                         "(defcustom mcg-vale-command \"vale\""
                         "(defun mcg-vale-flymake"
                         "(defun mcg-vale--maybe-enable"
                         "flymake-diagnostic-functions"
                         "markdown-mode-hook"
                         "org-mode-hook"
                         "text-mode-hook"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents example-source)
      (dolist (snippet '("Enable Vale prose diagnostics"
                         "Default: nil"
                         "(setq mcg-vale-enable t)"
                         "(setq mcg-vale-command \"vale\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-zig-deferred-trigger-uses-safe-mode-dispatch ()
  (let ((modules-source
         (expand-file-name "config-org/core/core-modules.org" mcg-emacs-dir))
        (zig-source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents modules-source)
      (should (search-forward "((:lang . zig) . (\"\\\\.zig\\\\'\" . mcg-zig-deferred-mode))" nil t)))
    (with-temp-buffer
      (insert-file-contents zig-source)
      (dolist (snippet '("defun mcg-zig-deferred-mode"
                         "zig-ts-mode"
                         "treesit-language-available-p"
                         "zig-mode"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-zig-format-keybinding-matches-help ()
  (let ((source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(define-key map (kbd \"C-c C-f\") #'mcg-format-buffer)"
                         "C-c C-f      - Format buffer"
                         "C-c r f      - Format buffer"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-zig-formatting-has-local-fallback-command ()
  (let ((source
         (expand-file-name "config-org/modules/editor/init-format.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(zig-mode . mcg-format-zig-buffer)"
                         "(zig-ts-mode . mcg-format-zig-buffer)"
                         "(zig-mode . ((\"zig\" \"fmt\")))"
                         "(zig-ts-mode . ((\"zig\" \"fmt\")))"
                         "(defun mcg-format-zig-buffer"
                         "mcg-format-with-external-command"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-zig-build-commands-use-unified-compile-runner ()
  (let ((source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defun mcg-zig--compile"
                         "(mcg-compile--run command root)"
                         "(defun mcg-zig-build-command"
                         "(mcg-zig--compile (mcg-zig-build-command 'build))"
                         "(mcg-zig--compile (mcg-zig-build-command 'run))"
                         "(mcg-zig--compile (mcg-zig-build-command 'test))"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-zig-lsp-detects-zls-before-enabling-lsp ()
  (let ((source
         (expand-file-name "config-org/modules/lang/init-zig.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(defcustom mcg-zig-enable-lsp t"
                         "(defcustom mcg-zig-warn-missing-zls nil"
                         "(defun mcg-zig--zls-available-p"
                         "(executable-find \"zls\")"
                         "(defun mcg-zig-maybe-enable-lsp"
                         "(lsp-bridge-mode 1)"
                         "mcg-zig--missing-zls-warned"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-web-biome-is-opt-in-not-default-formatting ()
  (let ((format-source
         (expand-file-name "config-org/modules/editor/init-format.org" mcg-emacs-dir))
        (lsp-source
         (expand-file-name "config-org/modules/tools/init-lsp-bridge.org" mcg-emacs-dir))
        (example-source
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents lsp-source)
      (should (search-forward "(defcustom mcg-lsp-enable-biome nil" nil t)))
    (with-temp-buffer
      (insert-file-contents format-source)
      (should-not (search-forward "(\"biome\" \"format\"" nil t))
      (should (search-forward "mcg-format--web-external-candidates" nil t))
      (should (search-forward "mcg-format--web-project-has-antfu-eslint-p" nil t))
      (should (search-forward "mcg-format--web-project-has-prettier-p" nil t)))
    (with-temp-buffer
      (insert-file-contents example-source)
      (should (search-forward "Use biome for Web development" nil t))
      (should (search-forward "Default: nil" nil t))
      (should (search-forward "Default formatting prefers project ESLint" nil t))
      (should (search-forward "(setq mcg-lsp-enable-biome t)" nil t)))))

(ert-deftest mcg-lsp-bridge-side-effects-are-opt-in-by-default ()
  (let ((lsp-source
         (expand-file-name "config-org/modules/tools/init-lsp-bridge.org" mcg-emacs-dir))
        (example-source
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents lsp-source)
      (dolist (snippet '("(defcustom mcg-lsp-enable-inlay-hint nil"
                         "(defcustom mcg-lsp-enable-auto-format-code nil"
                         "(defcustom mcg-lsp-enable-org-babel nil"
                         "(setq lsp-bridge-enable-inlay-hint mcg-lsp-enable-inlay-hint)"
                         "(setq lsp-bridge-enable-auto-format-code mcg-lsp-enable-auto-format-code)"
                         "(setq lsp-bridge-enable-org-babel mcg-lsp-enable-org-babel)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t)))
      (dolist (snippet '("(setq lsp-bridge-enable-inlay-hint t)"
                         "(setq lsp-bridge-enable-auto-format-code t)"
                         "(setq lsp-bridge-enable-org-babel t)"))
        (goto-char (point-min))
        (should-not (search-forward snippet nil t))))
    (with-temp-buffer
      (insert-file-contents example-source)
      (dolist (snippet '("LSP Bridge side effects are opt-in"
                         "Default: nil"
                         "(setq mcg-lsp-enable-inlay-hint t)"
                         "(setq mcg-lsp-enable-auto-format-code t)"
                         "(setq mcg-lsp-enable-org-babel t)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-format-web-detects-antfu-eslint-before-prettier ()
  (require 'init-format)
  (let ((root (make-temp-file "mcg-web-antfu-format" t)))
    (write-region
     "{\"devDependencies\":{\"@antfu/eslint-config\":\"latest\",\"eslint\":\"latest\",\"prettier\":\"latest\"}}\n"
     nil
     (expand-file-name "package.json" root))
    (let ((default-directory root)
          (major-mode 'typescript-ts-mode))
      (should (mcg-format--web-project-has-antfu-eslint-p root))
      (should (mcg-format--web-project-has-eslint-p root))
      (should (mcg-format--web-project-has-prettier-p root))
      (should (equal "eslint" (caar (mcg-format--web-external-candidates))))
      (should-not (equal "prettier" (caar (mcg-format--web-external-candidates)))))))

(ert-deftest mcg-format-web-uses-prettier-only-when-project-declares-it ()
  (require 'init-format)
  (let ((plain-root (make-temp-file "mcg-web-plain-format" t))
        (prettier-root (make-temp-file "mcg-web-prettier-format" t)))
    (write-region "{\"devDependencies\":{\"typescript\":\"latest\"}}\n"
                  nil
                  (expand-file-name "package.json" plain-root))
    (write-region "{\"devDependencies\":{\"prettier\":\"latest\"}}\n"
                  nil
                  (expand-file-name "package.json" prettier-root))
    (let ((default-directory plain-root)
          (major-mode 'typescript-ts-mode))
      (should-not (mcg-format--web-project-has-eslint-p plain-root))
      (should-not (mcg-format--web-project-has-prettier-p plain-root))
      (should-not (mcg-format--web-external-candidates)))
    (let ((default-directory prettier-root)
          (major-mode 'typescript-ts-mode))
      (should (mcg-format--web-project-has-prettier-p prettier-root))
      (should (equal "prettier" (caar (mcg-format--web-external-candidates)))))))

(ert-deftest mcg-config-example-documents-python-dev-options ()
  (let ((source-example
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source-example)
      (dolist (setting '("mcg-lsp-python-server"
                         "mcg-python-test-command"
                         "mcg-python-ruff-command"
                         "mcg-python-detect-project-tools"
                         "uv.lock"
                         "poetry.lock"
                         "pdm.lock"
                         "python -m pytest"
                         "ruff"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-config-example-documents-zig-dev-options ()
  (let ((source-example
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source-example)
      (dolist (setting '("Enable Zig LSP when zls is available"
                         "Default: mcg-zig-enable-lsp = t"
                         "Default: mcg-zig-warn-missing-zls = nil"
                         "(setq mcg-zig-enable-lsp nil)"
                         "(setq mcg-zig-warn-missing-zls t)"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-config-example-documents-org-safety-and-appearance-options ()
  (let ((source-example
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source-example)
      (dolist (setting '("mcg-org-babel-confirm-evaluate"
                         "mcg-initial-frame-background"
                         "mcg-initial-frame-foreground"
                         ":load idle"
                         "mcg-org-export-babel-evaluate"
                         "mcg-org-babel-load-languages"
                         "mcg-org-babel-enable-zig"
                         "mcg-org-babel-src-block-languages"
                         "mcg-org-todo-keywords"
                         "mcg-org-todo-keyword-faces"
                         "mcg-org-ellipsis"
                         "mcg-org-modern-todo-faces"
                         "mcg-org-download-default-image-width"
                         "mcg-org-download-image-directory-suffix"
                         "mcg-calendar-location-longitude"
                         "mcg-org-agenda-use-time-grid"
                         "mcg-org-agenda-tags-column"
                         "mcg-org-agenda-custom-commands"
                         "Hugo integration is opt-in"
                         "mcg-org-hugo-enable"
                         "(setq mcg-org-hugo-enable t)"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-config-example-documents-dashboard-options ()
  (let ((source-example
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source-example)
      (dolist (setting '("mcg-dashboard-enabled"
                         "mcg-dashboard-startup"
                         "mcg-dashboard-items"
                         "mcg-dashboard-icon-type"
                         "mcg-dashboard-startup-banner"
                         "mcg-dashboard-banner-ascii"
                         "mcg-dashboard-footer-messages"
                         "mcg-dashboard-nerd-icons-heading-icons"
                         "nerd-icons"))
        (goto-char (point-min))
        (should (search-forward setting nil t))))))

(ert-deftest mcg-tangle-does-not-leave-generated-el-files-in-config-org ()
  (let* ((source-dir (expand-file-name "config-org" mcg-emacs-dir))
         (generated-files
          (mapcar (lambda (file) (file-relative-name file source-dir))
                  (directory-files-recursively source-dir "\\.el\\'"))))
    (should (equal nil generated-files))))

(ert-deftest mcg-makefile-tangles-org-through-temp-directory ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("(make-temp-file (string ?m ?c ?g ?- ?t ?a ?n ?g ?l ?e ?-) t)"
                         "(org-babel-tangle-file temp-source)"
                         "(copy-file generated target t)"
                         "(delete-directory temp-dir t)"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-makefile-uses-windows-safe-eval-quoting ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (search-forward "--eval '" nil t)))))

(ert-deftest mcg-startup-profile-script-loads-generated-config ()
  (let ((path (expand-file-name "scripts/profile-startup.el" mcg-emacs-dir)))
    (should (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("mcg-profile-mode"
                         "site-lisp/config/early-init.el"
                         "site-lisp/config/init.el"
                         "mcg-profile-startup-print-report"
                         "mcg-load-times"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-startup-profile-script-suppresses-third-party-noise ()
  (let ((path (expand-file-name "scripts/profile-startup.el" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("inhibit-message"
                         "warning-minimum-level"
                         ":error"
                         "byte-compile-warnings"
                         "native-comp-async-report-warnings-errors"
                         "mcg-profile-startup-quiet-output"
                         "advice-add"
                         "display-warning"
                         "byte-compile-warn"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-startup-profile-script-does-not-highlight-disabled-dashboard ()
  (let ((path (expand-file-name "scripts/profile-startup.el" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should (search-forward "mcg-profile-startup-key-modules" nil t))
      (should-not (search-forward "\":ui/dashboard\"" nil t))
      (should-not (search-forward "\":tools/blink-search\"" nil t))
      (dolist (snippet '("\":ui/pulsar\""
                         "\":completion/embark\""))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-makefile-exposes-startup-profile-target ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("profile-startup:"
                         "scripts/profile-startup.el"
                         "profile-startup-stderr.log"
                         "make profile-startup"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-readmes-document-startup-profile-command ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (should (search-forward "make profile-startup" nil t))))))

(ert-deftest mcg-readmes-document-submodule-recovery-for-missing-extensions ()
  (let ((cases '(("README.org"
                  "vundo"
                  "org-modern"
                  "org-appear"
                  "cond-let"
                  "magit-section"
                  "git submodule sync --recursive"
                  "git submodule update --init --recursive"
                  "make all")
                 ("README_CN.org"
                  "vundo"
                  "org-modern"
                  "org-appear"
                  "cond-let"
                  "magit-section"
                  "git submodule sync --recursive"
                  "git submodule update --init --recursive"
                  "make all"))))
    (dolist (case cases)
      (let ((path (expand-file-name (car case) mcg-emacs-dir)))
        (with-temp-buffer
          (insert-file-contents path)
          (dolist (snippet (cdr case))
            (goto-char (point-min))
            (should (search-forward snippet nil t))))))))

(ert-deftest mcg-readmes-document-current-startup-loading-strategy ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (dolist (snippet '("Immediate"
                           "Idle"
                           "Deferred"
                           "awesome-tray"
                           "org"
                           "scratch"
                           "make profile-startup"))
          (goto-char (point-min))
          (should (search-forward snippet nil t)))))))

(ert-deftest mcg-readmes-document-current-enabled-modules ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (dolist (snippet '(":ui"
                           "ef-themes"
                           "nerd-icons"
                           "-dashboard"
                           "pulsar"
                           ":editor"
                           "hl-todo"
                           "ibuffer"
                           "treesit"
                           "dirvish"
                           ":completion"
                           "consult"
                           "marginalia"
                           "orderless"
                           "embark"
                           ":tools"
                           "project"
                           "diagnostics"
                           "lsp-bridge"
                           "blink-search"
                           "color-rg"
                           "majutsu"
                           "clutch"
                           ":enhance"
                           "help"
                           "recentf"
                           ":org"
                           "org"
                           ":writing"
                           "markdown"
                           "vale"
                           ":lang"
                           "python"
                           "zig"
                           "web"))
          (goto-char (point-min))
          (should (search-forward snippet nil t)))
        (dolist (stale-snippet '("magit search rime tabs eshell"
                                 "lsp rust python typescript web cpp lua zig"
                                 "Goto (Avy)"
                                 "快速切换 (ace)"
                                 "which-key descriptions"
                                 "custom.el"
                                 "mcg-theme"
                                 "mcg-font-size"))
          (goto-char (point-min))
          (should-not (search-forward stale-snippet nil t)))))))

(ert-deftest mcg-readmes-document-writing-and-doctor-behavior ()
  (let ((cases '(("README.org"
                  "Markdown is deferred"
                  "Vale is optional"
                  "mcg-vale-enable nil"
                  "External Dev Tools"
                  "Development Environments"
                  "Tree-sitter Grammars"
                  "mcg-python-detect-project-tools"
                  "uv.lock"
                  "poetry.lock"
                  "pdm.lock"
                  "build.zig"
                  "Antfu ESLint")
                 ("README_CN.org"
                  "Markdown 按需加载"
                  "Vale 默认关闭"
                  "mcg-vale-enable nil"
                  "外部开发工具"
                  "Development Environments"
                  "Tree-sitter Grammars"
                  "mcg-python-detect-project-tools"
                  "uv.lock"
                  "poetry.lock"
                  "pdm.lock"
                  "build.zig"
                  "Antfu ESLint"))))
    (dolist (case cases)
      (let ((path (expand-file-name (car case) mcg-emacs-dir)))
        (with-temp-buffer
          (insert-file-contents path)
          (dolist (snippet (cdr case))
            (goto-char (point-min))
            (should (search-forward snippet nil t))))))))

(ert-deftest mcg-readmes-document-python-compile-tool-detection ()
  (let ((cases '(("README.org"
                  "C-c c t"
                  "mcg-python-detect-project-tools"
                  "uv run"
                  "poetry run"
                  "pdm run"
                  "build.zig"
                  "zig build test"
                  "zig build run")
                 ("README_CN.org"
                  "C-c c t"
                  "mcg-python-detect-project-tools"
                  "uv run"
                  "poetry run"
                  "pdm run"
                  "build.zig"
                  "zig build test"
                  "zig build run"))))
    (dolist (case cases)
      (let ((path (expand-file-name (car case) mcg-emacs-dir)))
        (with-temp-buffer
          (insert-file-contents path)
          (dolist (snippet (cdr case))
            (goto-char (point-min))
            (should (search-forward snippet nil t))))))))

(ert-deftest mcg-readmes-document-lazy-command-entries-for-idle-modules ()
  (let ((cases '(("README.org"
                  "lazy command entries"
                  "compile, magit, vundo"
                  "blink-search, color-rg, dirvish"
                  "org capture"
                  "clutch"
                  "majutsu"
                  "available immediately"
                  "loaded on first use")
                 ("README_CN.org"
                  "lazy command entry"
                  "compile、magit、vundo"
                  "blink-search、color-rg、dirvish"
                  "org capture、clutch、majutsu"
                  "启动后立即可用"
                  "首次调用时加载"))))
    (dolist (case cases)
      (let ((path (expand-file-name (car case) mcg-emacs-dir)))
        (with-temp-buffer
          (insert-file-contents path)
          (dolist (snippet (cdr case))
            (goto-char (point-min))
            (should (search-forward snippet nil t))))))))

(ert-deftest mcg-readmes-document-current-org-user-surface ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (dolist (snippet '("Org Mode"
                           "C-c n c"
                           "C-c n a"
                           "C-c n l"
                           "C-c n t"
                           "C-c n v"
                           "C-c C-v C-b"
                           "C-c C-v C-r"
                           "C-c C-v i"
                           "mcg-org-initialize-directories"
                           "mcg/org-open-notes"
                           "mcg/org-open-agenda-dir"
                           "mcg-org-babel-confirm-evaluate"
                           "mcg-org-export-babel-evaluate"
                           "mcg-org-babel-enable-zig"
                           "mcg-org-hugo-enable"
                           "mcg-org-hugo-enable nil"
                           "(setq mcg-org-hugo-enable t)"
                           "mcg-org-blog-capture-directory"))
          (goto-char (point-min))
          (should (search-forward snippet nil t)))
        (dolist (stale-snippet '("mcg-org-show-status"
                                 "mcg-org-list-modules"
                                 "mcg-org-load-module"
                                 "mcg-org-reload-module"))
          (goto-char (point-min))
          (should-not (search-forward stale-snippet nil t)))))))

(ert-deftest mcg-init-paths-infers-emacs-root-from-core-bootstrap-location ()
  (let ((expected (file-name-as-directory (expand-file-name "." default-directory)))
        (mcg-emacs-dir nil)
        (mcg-config-dir nil)
        (mcg-core-dir nil)
        (mcg-cache-dir nil)
        (mcg-data-dir nil))
    (dolist (var '(savehist-file save-place-file recentf-save-file))
      (makunbound var))
    (mcg-init-paths)
    (should (equal expected mcg-emacs-dir))
    (should (equal (expand-file-name "site-lisp/config/" expected)
                   mcg-config-dir))
    (should (equal (expand-file-name ".cache/" expected)
                   mcg-cache-dir))
    (should (equal (expand-file-name ".cache/savehist.el" expected)
                   (symbol-value 'savehist-file)))
    (should (equal (expand-file-name ".cache/places.el" expected)
                   (symbol-value 'save-place-file)))
    (should (equal (expand-file-name ".cache/recentf.el" expected)
                   (symbol-value 'recentf-save-file)))))

(ert-deftest mcg-init-paths-redirects-eln-cache-into-config-cache ()
  (skip-unless (boundp 'native-comp-eln-load-path))
  (let* ((expected (file-name-as-directory (expand-file-name "." default-directory)))
         (expected-eln-cache (expand-file-name ".cache/eln-cache/" expected))
         (system-native-lisp (expand-file-name "system-native-lisp/" temporary-file-directory))
         (original-eln-load-path (symbol-value 'native-comp-eln-load-path))
         (mcg-emacs-dir nil)
         (mcg-config-dir nil)
         (mcg-core-dir nil)
         (mcg-cache-dir nil)
         (mcg-data-dir nil))
    (unwind-protect
        (progn
          (set 'native-comp-eln-load-path
               (list (expand-file-name ".emacs.d/eln-cache/" temporary-file-directory)
                     system-native-lisp))
          (mcg-init-paths)
          (should (equal expected-eln-cache
                         (car (symbol-value 'native-comp-eln-load-path))))
          (should (member system-native-lisp
                          (cdr (symbol-value 'native-comp-eln-load-path))))
          (should (file-directory-p expected-eln-cache)))
      (set 'native-comp-eln-load-path original-eln-load-path))))

(ert-deftest mcg-private-config-does-not-keep-inline-passwords ()
  (dolist (relative-path '("config-org/private/config-example.org"
                           "site-lisp/config/private/config-example.el"))
    (let ((path (expand-file-name relative-path mcg-emacs-dir)))
      (when (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (should-not (re-search-forward ":password[[:space:]\n]+\"" nil t)))))))

(ert-deftest mcg-private-loader-loads-single-config-file ()
  (let ((path (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should (search-forward "config.el" nil t))
      (dolist (snippet '("local-settings.el"
                         "local-secrets.el"))
        (goto-char (point-min))
        (should-not (search-forward snippet nil t))))))

(ert-deftest mcg-private-config-file-is-gitignored ()
  (let ((gitignore (expand-file-name "../.gitignore" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents gitignore)
      (should (search-forward "emacs/site-lisp/config/private/config.el"
                              nil t)))))

(ert-deftest mcg-make-all-creates-minimal-private-config-file ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should (search-forward "ensure-private-config-file:" nil t))
      (should (search-forward "all: ensure-private-config-file" nil t))
      (dolist (snippet '("DST_DIR = site-lisp/config"
                         "$(DST_DIR)/private/config.el"
                         "if test -f $(DST_DIR)/private/config.el"
                         "if (Test-Path '$(DST_DIR)/private/config.el')"
                         "lexical-binding: t"
                         "Put machine-local settings and secrets here"
                         "Preserving existing private config.el"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-private-config-file-is-never-overwritten-by-generation ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should-not (search-forward "$(DST_DIR)/private/config.el: $(SRC_DIR)" nil t))
      (should-not (search-forward "config-example.el\" \"site-lisp/config/private/config.el\"" nil t))
      (should-not (search-forward "config-example.org\" \"site-lisp/config/private/config.el\"" nil t)))))

(ert-deftest mcg-config-example-documents-single-private-config-file ()
  (let ((path (expand-file-name "config-org/private/config-example.org"
                                mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("Local Configuration"
                         "site-lisp/config/private/config.el"
                         "settings and secrets"))
        (goto-char (point-min))
        (should (search-forward snippet nil t)))
      (dolist (snippet '("local-settings.el"
                         "local-secrets.el"
                         "Copy this file to ~config.org~"))
        (goto-char (point-min))
        (should-not (search-forward snippet nil t))))))

(ert-deftest mcg-readme-does-not-require-private-config-org-copy ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should-not
         (search-forward
          "cp config-org/private/config-example.org config-org/private/config.org"
          nil t))
        (should (search-forward "site-lisp/config/private/config.el" nil t))
        (goto-char (point-min))
        (should-not (search-forward "local-settings.el" nil t))
        (goto-char (point-min))
        (should-not (search-forward "local-secrets.el" nil t))))))

(ert-deftest mcg-readmes-document-hl-todo-pulsar-and-ibuffer ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (dolist (snippet '("hl-todo"
                           "pulsar"
                           "ibuffer"
                           "C-c s T"
                           "C-c b i"
                           "C-c b p"))
          (goto-char (point-min))
          (should (search-forward snippet nil t)))))))

(ert-deftest mcg-state-cache-directory-is-gitignored ()
  (let ((gitignore (expand-file-name "../.gitignore" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents gitignore)
      (should (search-forward "emacs/.cache/" nil t)))))

(ert-deftest mcg-batch-load-does-not-regress-owned-warning-budget ()
  (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
         (output-buffer (generate-new-buffer " *mcg-batch-load-output*"))
         (status nil))
    (unwind-protect
        (progn
          (setq status
                (call-process emacs-bin nil (list output-buffer t) nil
                              "-Q" "--batch"
                              "-L" "site-lisp/config"
                              "-L" "site-lisp/config/core"
                              "-l" "site-lisp/config/init.el"))
          (should (= 0 status))
          (with-current-buffer output-buffer
            (let ((output (buffer-string)))
              (dolist (forbidden '("~/.emacs.d/history"
                                   "recentf.eld"
                                   "site-lisp/config/private/config.el\"."
                                   "nerd-icons font missing"))
                (should-not (string-match-p (regexp-quote forbidden) output))))))
      (kill-buffer output-buffer))))

(ert-deftest mcg-readme-states-personal-distribution-positioning ()
  (dolist (case '(("README.org" . "personal Emacs distribution")
                  ("README_CN.org" . "个人 Emacs 发行版")))
    (let ((path (expand-file-name (car case) mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward (cdr case) nil t))))))

(ert-deftest mcg-readme-does-not-claim-built-in-first-positioning ()
  (dolist (case '(("README.org" . "Built-in First")
                  ("README_CN.org" . "内置优先")))
    (let ((path (expand-file-name (car case) mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should-not (search-forward (cdr case) nil t))))))

(ert-deftest mcg-readme-links-only-existing-config-docs ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should-not (search-forward "config-org/README" nil t))))))

(ert-deftest mcg-readmes-link-standalone-keybindings-reference ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward "KEYBINDINGS.org" nil t))))))

(ert-deftest mcg-keybindings-reference-separates-global-and-mode-local-bindings ()
  (let ((path (expand-file-name "KEYBINDINGS.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (snippet '("* Global Bindings"
                         "* Mode-local Bindings"
                         "** Org"
                         "** LSP Bridge"
                         "** Python"
                         "** Zig"
                         "** Web"
                         "** YASnippet"
                         "** Dirvish"
                         "** Vundo"
                         "| C-c s b | blink-search | Lazy entry |"
                         "| C-c n c | org-capture | Capture |"
                         "| C-x C-b | ibuffer | Ibuffer |"))
        (goto-char (point-min))
        (should (search-forward snippet nil t)))
      (dolist (stale-snippet '("C-c t | sort-tab prefix"
                               "** Clutch / Org"))
        (goto-char (point-min))
        (should-not (search-forward stale-snippet nil t))))))

(ert-deftest mcg-top-level-entrypoint-file-exists-and-loads-generated-init ()
  (let ((path (expand-file-name "init.el" mcg-emacs-dir)))
    (should (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (should (search-forward "lexical-binding: t" (line-end-position) t))
      (should (search-forward "site-lisp/config/init.el" nil t)))))

(ert-deftest mcg-top-level-entrypoint-is-built-by-make-all ()
  (let ((path (expand-file-name "Makefile" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should (search-forward "init.el: config-org/root-init.org" nil t))
      (should (search-forward "early-init.el: config-org/early-init.org" nil t))
      (should (search-forward
               "all: ensure-private-config-file init.el early-init.el"
               nil t)))))

(ert-deftest mcg-top-level-early-init-file-exists-for-symlink-install ()
  (let ((path (expand-file-name "early-init.el" mcg-emacs-dir)))
    (should (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (should (search-forward "lexical-binding: t" (line-end-position) t))
      (should (search-forward "startup-redirect-eln-cache" nil t)))))

(ert-deftest mcg-top-level-entrypoint-is-documented-for-manual-load ()
  (dolist (readme '("README.org" "README_CN.org"))
    (let ((path (expand-file-name readme mcg-emacs-dir)))
      (with-temp-buffer
        (insert-file-contents path)
        (should (search-forward "(load-file \"~/dotfiles/emacs/init.el\")" nil t))
        (should (search-forward "~/.emacs.d" nil t))
        (should-not (search-forward "(load-file \"~/dotfiles/emacs/site-lisp/config/init.el\")" nil t))))))

(ert-deftest mcg-module-declaration-registers-enabled-disabled-and-load-order ()
  (let ((mcg-modules-alist nil)
        (mcg-disabled-modules nil)
        (mcg-module-load-order nil)
        (mcg-module-metadata-alist nil))
    (mcg! :ui font -holo-layer
          :tools magit color-rg
          :lang zig)
    (should (equal mcg-modules-alist
                   '((:ui . (font))
                     (:tools . (magit color-rg))
                     (:lang . (zig)))))
    (should (equal mcg-disabled-modules
                   '((:ui . (holo-layer)))))
    (should (equal mcg-module-load-order
                   '((:ui . font)
                     (:tools . magit)
                     (:tools . color-rg)
                     (:lang . zig))))
    (should (equal (mcg-module-load-strategy :ui 'font) 'immediate))
    (should (equal (mcg-module-load-strategy :tools 'magit) 'immediate))))

(ert-deftest mcg-module-declaration-records-load-strategy-metadata ()
  (let ((mcg-modules-alist nil)
        (mcg-disabled-modules nil)
        (mcg-module-load-order nil)
        (mcg-module-metadata-alist nil))
    (mcg! :ui font (dashboard :load immediate) -holo-layer
          :tools project (magit :load idle)
          :writing (markdown :load deferred))
    (should (equal mcg-modules-alist
                   '((:ui . (font dashboard))
                     (:tools . (project magit))
                     (:writing . (markdown)))))
    (should (equal mcg-disabled-modules
                   '((:ui . (holo-layer)))))
    (should (equal mcg-module-load-order
                   '((:ui . font)
                     (:ui . dashboard)
                     (:tools . project)
                     (:tools . magit)
                     (:writing . markdown))))
    (should (equal (mcg-module-load-strategy :ui 'font) 'immediate))
    (should (equal (mcg-module-load-strategy :tools 'magit) 'idle))
    (should (equal (mcg-module-load-strategy :writing 'markdown) 'deferred))
    (should (equal (mcg-list-modules-by-load-strategy 'immediate)
                   '((:ui . font)
                     (:ui . dashboard)
                     (:tools . project))))
    (should (equal (mcg-list-modules-by-load-strategy 'idle)
                   '((:tools . magit))))
    (should (equal (mcg-list-modules-by-load-strategy 'deferred)
                   '((:writing . markdown))))))

(ert-deftest mcg-load-module-loads-module-file-and-records-status ()
  (let* ((root (make-temp-file "mcg-module-test" t))
         (mcg-modules-dir (expand-file-name "modules" root))
         (mcg-loaded-modules nil)
         (mcg-module-load-errors nil)
         (mcg-module-error-traces nil)
         (mcg-profile-mode nil)
         (module-dir (expand-file-name "test" mcg-modules-dir))
         (module-file (expand-file-name "init-alpha.el" module-dir)))
    (make-directory module-dir t)
    (write-region
     ";;; init-alpha.el --- test module -*- lexical-binding: t; -*-\n(setq mcg-test-loaded-module 'loaded)\n(provide 'init-alpha)\n"
     nil module-file)
    (should (mcg-load-module :test 'alpha))
    (should (eq (symbol-value 'mcg-test-loaded-module) 'loaded))
    (should (member '(:test . alpha) mcg-loaded-modules))
    (should-not mcg-module-load-errors)))

(ert-deftest mcg-idle-load-does-not-mark-failed-module-as-loaded ()
  (let* ((root (make-temp-file "mcg-idle-module-test" t))
         (mcg-modules-dir (expand-file-name "modules" root))
         (mcg-loaded-modules nil)
         (mcg-idle-load-modules '((:test . missing)))
         (mcg-idle-loaded-modules nil)
         (mcg-idle-load-timer nil)
         (mcg-module-load-errors nil)
         (mcg-module-error-traces nil))
    (mcg--idle-load-next)
    (should-not (member '(:test . missing) mcg-loaded-modules))
    (should-not (member '(:test . missing) mcg-idle-loaded-modules))
    (should (assoc '(:test . missing) mcg-module-load-errors))))

(ert-deftest mcg-lazy-module-command-loads-module-through-module-system ()
  (let* ((root (make-temp-file "mcg-lazy-module-test" t))
         (mcg-modules-dir (expand-file-name "modules" root))
         (mcg-loaded-modules nil)
         (mcg-module-load-errors nil)
         (mcg-module-error-traces nil)
         (mcg-profile-mode nil)
         (module-dir (expand-file-name "test" mcg-modules-dir))
         (module-file (expand-file-name "init-lazy.el" module-dir))
         (command 'mcg-test-lazy-command))
    (make-directory module-dir t)
    (when (fboundp command)
      (fmakunbound command))
    (write-region
     ";;; init-lazy.el --- test lazy module -*- lexical-binding: t; -*-\n(defun mcg-test-lazy-command ()\n  (interactive)\n  (setq mcg-test-lazy-command-called t))\n(provide 'init-lazy)\n"
     nil module-file)
    (mcg-define-lazy-module-command :test 'lazy command)
    (should (fboundp command))
    (call-interactively command)
    (should (member '(:test . lazy) mcg-loaded-modules))
    (should (bound-and-true-p mcg-test-lazy-command-called))))

(ert-deftest mcg-bind-lazy-module-key-does-not-conflict-with-real-binding ()
  (let* ((root (make-temp-file "mcg-lazy-key-test" t))
         (mcg-modules-dir (expand-file-name "modules" root))
         (mcg-loaded-modules nil)
         (mcg-module-load-errors nil)
         (mcg-module-error-traces nil)
         (mcg-keybinding-registry (make-hash-table :test 'equal))
         (mcg-keybinding-conflicts nil)
         (mcg-keybinding-conflict-policy 'reject)
         (mcg-profile-mode nil)
         (keymap (make-sparse-keymap))
         (module-dir (expand-file-name "test" mcg-modules-dir))
         (module-file (expand-file-name "init-lazy-key.el" module-dir))
         (command 'mcg-test-lazy-key-command))
    (make-directory module-dir t)
    (when (fboundp command)
      (fmakunbound command))
    (write-region
     ";;; init-lazy-key.el --- test lazy key module -*- lexical-binding: t; -*-\n(require 'core-lib)\n(defun mcg-test-lazy-key-command ()\n  (interactive)\n  (setq mcg-test-lazy-key-command-called t))\n(mcg-bind-key \"C-c l\" #'mcg-test-lazy-key-command \"test/lazy-key\" mcg-test-lazy-keymap)\n(provide 'init-lazy-key)\n"
     nil module-file)
    (setq mcg-test-lazy-keymap keymap)
    (mcg-bind-lazy-module-key "C-c l" command "test/lazy-key" :test 'lazy-key keymap)
    (call-interactively command)
    (should (bound-and-true-p mcg-test-lazy-key-command-called))
    (should (equal (mcg-lookup-keybinding "C-c l")
                   (cons command "test/lazy-key")))
    (should-not mcg-keybinding-conflicts)))

(ert-deftest mcg-editor-state-files-live-under-mcg-cache-directory ()
  (require 'init-defaults)
  (let* ((root (make-temp-file "mcg-editor-state" t))
         (mcg-cache-dir (file-name-as-directory root)))
    (cl-letf (((symbol-function 'savehist-mode) #'ignore)
              ((symbol-function 'save-place-mode) #'ignore))
      (mcg-editor-setup-savehist)
      (mcg-editor-setup-saveplace))
    (should (equal (expand-file-name "savehist.el" root)
                   savehist-file))
    (should (equal (expand-file-name "places.el" root)
                   save-place-file))))

(ert-deftest mcg-editor-backup-state-files-live-under-mcg-cache-directory ()
  (require 'init-defaults)
  (let* ((root (make-temp-file "mcg-editor-backups" t))
         (mcg-cache-dir (file-name-as-directory root)))
    (mcg-editor-setup-backups)
    (should (equal `((".*" . ,(expand-file-name "backups/" root)))
                   backup-directory-alist))
    (should (equal `((".*" ,(expand-file-name "auto-save/" root) t))
                   auto-save-file-name-transforms))))

(ert-deftest mcg-editor-savehist-strips-kill-ring-text-properties-before-save ()
  (require 'init-defaults)
  (let ((kill-ring (list (propertize "alpha" 'face 'bold)
                         'not-a-string
                         (propertize "beta" 'font-lock-face 'warning))))
    (mcg-editor-clean-savehist-kill-ring)
    (should (equal kill-ring '("alpha" "beta")))
    (dolist (entry kill-ring)
      (should-not (text-properties-at 0 entry))))
  (should (memq #'mcg-editor-clean-savehist-kill-ring savehist-save-hook)))

(ert-deftest mcg-editor-switch-to-buffer-follows-display-actions ()
  (require 'init-defaults)
  (should (eq switch-to-buffer-in-dedicated-window 'pop))
  (should switch-to-buffer-obey-display-actions))

(ert-deftest mcg-hl-todo-module-is-registered-as-editor ()
  (let ((source (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "hl-todo" nil t)))))

(ert-deftest mcg-hl-todo-enables-global-keyword-highlighting ()
  (require 'init-hl-todo)
  (should (bound-and-true-p global-hl-todo-mode))
  (should (memq 'prog-mode hl-todo-include-modes))
  (should (memq 'conf-mode hl-todo-include-modes))
  (should (memq 'org-mode hl-todo-exclude-modes))
  (dolist (keyword '("TODO" "FIXME" "BUG" "HACK" "NOTE" "REVIEW" "DEPRECATED"))
    (should (assoc keyword hl-todo-keyword-faces))))

(ert-deftest mcg-hl-todo-exposes-project-search-on-shared-search-prefix ()
  (let ((source (expand-file-name "config-org/modules/editor/init-hl-todo.org"
                                  mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "(defun mcg/hl-todo-search-project" nil t))
      (goto-char (point-min))
      (should (search-forward "(mcg-bind-key \"C-c s T\" #'mcg/hl-todo-search-project \"editor/hl-todo\")"
                              nil t)))))

(ert-deftest mcg-hl-todo-project-search-uses-current-project-root ()
  (require 'init-hl-todo)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-project-root-or-default)
               (lambda () "/tmp/mcg-project/"))
              ((symbol-function 'hl-todo-rgrep)
               (lambda (regexp files dir &optional confirm)
                 (setq called (list regexp files dir confirm)))))
      (mcg/hl-todo-search-project)
      (should (equal (nth 1 called) "*"))
      (should (equal (nth 2 called) "/tmp/mcg-project/"))
      (should-not (nth 3 called))
      (should (string-match-p "TODO" (car called)))
      (should (string-match-p "FIXME" (car called))))))

(ert-deftest mcg-ibuffer-module-is-registered-as-editor ()
  (let ((source (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (should (search-forward "ibuffer" nil t)))))

(ert-deftest mcg-ibuffer-binds-global-and-project-buffer-lists ()
  (let ((source (expand-file-name "config-org/modules/editor/init-ibuffer.org"
                                  mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source)
      (dolist (snippet '("(mcg-bind-key \"C-x C-b\" #'ibuffer \"editor/ibuffer\")"
                         "(mcg-bind-key \"C-c b i\" #'ibuffer \"editor/ibuffer\")"
                         "(mcg-bind-key \"C-c b p\" #'mcg/ibuffer-project \"editor/ibuffer\")"))
        (goto-char (point-min))
        (should (search-forward snippet nil t))))))

(ert-deftest mcg-ibuffer-project-filters-current-project-files ()
  (require 'init-ibuffer)
  (let ((called nil))
    (cl-letf (((symbol-function 'mcg-project-root-or-default)
               (lambda () "/tmp/mcg-project/"))
              ((symbol-function 'ibuffer)
               (lambda (&rest args)
                 (setq called (list :ibuffer args))))
              ((symbol-function 'ibuffer-filter-by-filename)
               (lambda (qualifier)
                 (setq called (append called (list :filter qualifier))))))
      (mcg/ibuffer-project)
      (should (equal (plist-get called :ibuffer)
                     '(nil "*Ibuffer: mcg-project*")))
      (should (equal (plist-get called :filter)
                     (regexp-quote "/tmp/mcg-project/"))))))

(ert-deftest mcg-editor-setup-backups-is-called-by-defaults-initialization ()
  (let ((path (expand-file-name "site-lisp/config/modules/editor/init-defaults.el"
                                mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (should (search-forward "mcg-editor-setup-backups" nil t)))))

(ert-deftest mcg-recentf-state-file-lives-under-mcg-cache-directory ()
  (require 'init-recentf)
  (let* ((root (make-temp-file "mcg-recentf-state" t))
         (mcg-cache-dir (file-name-as-directory root)))
    (cl-letf (((symbol-function 'recentf-mode) #'ignore))
      (mcg--setup-recentf))
    (should (equal (expand-file-name "recentf.el" root)
                   recentf-save-file))))

(ert-deftest mcg-recentf-save-file-is-set-before-requiring-recentf ()
  (let ((path (expand-file-name "site-lisp/config/modules/enhance/init-recentf.el"
                                mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((save-file-position (progn
                                  (goto-char (point-min))
                                  (search-forward "(setq recentf-save-file" nil t)))
            (require-position (progn
                                (goto-char (point-min))
                                (search-forward "(require 'recentf)" nil t))))
        (should save-file-position)
        (should require-position)
        (should (< save-file-position require-position))))))

(ert-deftest mcg-require-extension-finds-nested-extension-directory ()
  (let* ((root (make-temp-file "mcg-extension-test" t))
         (mcg-extensions-dir (expand-file-name "extensions" root))
         (mcg-cache-dir (expand-file-name "cache" root))
         (mcg-extension-cache nil)
         (mcg-extension-cache-file nil)
         (mcg-extension-cache-validate-signature t)
         (extension-dir (expand-file-name "tools/fake-ext" mcg-extensions-dir))
         (extension-file (expand-file-name "fake-ext.el" extension-dir)))
    (make-directory extension-dir t)
    (make-directory mcg-cache-dir t)
    (write-region
     ";;; fake-ext.el --- test extension -*- lexical-binding: t; -*-\n(defvar fake-ext-loaded t)\n(provide 'fake-ext)\n"
     nil extension-file)
    (unwind-protect
        (progn
          (should (mcg-require-extension 'fake-ext))
          (should (featurep 'fake-ext))
          (should (member extension-dir load-path)))
      (when (featurep 'fake-ext)
        (unload-feature 'fake-ext t)))))

(ert-deftest mcg-bind-key-rejects-conflicting-bindings-by-default ()
  (let ((mcg-keybinding-registry (make-hash-table :test 'equal))
        (mcg-keybinding-conflicts nil)
        (mcg-keybinding-conflict-policy 'reject)
        (keymap (make-sparse-keymap)))
    (should (mcg-bind-key "C-c t" #'ignore "first" keymap))
    (should-not (mcg-bind-key "C-c t" #'identity "second" keymap))
    (should (equal (mcg-lookup-keybinding "C-c t")
                   (cons #'ignore "first")))
    (should (eq (lookup-key keymap (kbd "C-c t")) #'ignore))
    (should (= 1 (length mcg-keybinding-conflicts)))))

;;; run-tests.el ends here
