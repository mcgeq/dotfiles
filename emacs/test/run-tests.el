;;; run-tests.el --- Tests for MCG Emacs config -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "site-lisp/config/core" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/org" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/ui" default-directory))

(defvar mcg-emacs-dir default-directory)
(defvar mcg-extensions-dir (expand-file-name "site-lisp/extensions" default-directory))
(defvar mcg-org-directory (expand-file-name "org" temporary-file-directory))
(defvar mcg-org-agenda-directory (expand-file-name "agenda" mcg-org-directory))
(defvar mcg-org-notes-directory (expand-file-name "notes" mcg-org-directory))
(defvar mcg-org-default-notes-file (expand-file-name "notes.org" mcg-org-notes-directory))
(defvar mcg-org-task-file (expand-file-name "tasks.org" mcg-org-agenda-directory))

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

(ert-deftest mcg-org-hugo-templates-are-added-after-base-capture-templates ()
  (cl-letf (((symbol-function 'mcg-require-extension)
             (lambda (&rest _args) nil)))
    (require 'init-org-hugo))
  (should (assoc "n" org-capture-templates))
  (should (assoc "j" org-capture-templates))
  (should (assoc "h" org-capture-templates))
  (should (assoc "hb" org-capture-templates))
  (dolist (key '("hl" "hla" "hle" "hlr" "hlv" "he"))
    (should-not (assoc key org-capture-templates))))

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
  (setq mcg-org-hugo-capture--state '(:title "Old post" :slug "old-post"))
  (should (fboundp 'mcg-org-hugo-capture-reset-state))
  (should (memq #'mcg-org-hugo-capture-reset-state org-capture-after-finalize-hook))
  (mcg-org-hugo-capture-reset-state)
  (should-not mcg-org-hugo-capture--state))

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
  (dolist (key '("hb"))
    (should (eq (nth 2 (mcg-test-capture-template key)) 'plain))))

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
        (should (search-forward setting nil t))))))

(ert-deftest mcg-startup-heavy-modules-are-idle-by-default ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (dolist (setting '("mcg-idle-modules"
                         "(:tools . magit)"
                         "(:editor . dirvish)"
                         "(:tools . clutch)"
                         "(:ui . holo-layer)"))
        (goto-char (point-min))
        (should (search-forward setting nil t)))
      (goto-char (point-min))
      (should (search-forward ":org" nil t)))))

(ert-deftest mcg-dashboard-module-is-registered-after-nerd-icons ()
  (let ((init-source
         (expand-file-name "config-org/init.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents init-source)
      (goto-char (point-min))
      (should (search-forward "nerd-icons" nil t))
      (let ((nerd-icons-position (point)))
        (should (search-forward "dashboard" nil t))
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
  (let ((dashboard-setup-called nil))
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
               (lambda () (setq dashboard-setup-called t))))
      (require 'init-dashboard)
      (should dashboard-setup-called)
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

(ert-deftest mcg-config-example-documents-org-safety-and-appearance-options ()
  (let ((source-example
         (expand-file-name "config-org/private/config-example.org" mcg-emacs-dir)))
    (with-temp-buffer
      (insert-file-contents source-example)
      (dolist (setting '("mcg-org-babel-confirm-evaluate"
                         "mcg-initial-frame-background"
                         "mcg-initial-frame-foreground"
                         "mcg-idle-modules"
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
                         "mcg-org-agenda-custom-commands"))
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

;;; run-tests.el ends here
