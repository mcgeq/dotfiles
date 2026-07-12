;;; run-tests.el --- Tests for MCG Emacs config -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "site-lisp/config/core" default-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/config/modules/org" default-directory))

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

(ert-deftest mcg-tangle-does-not-leave-generated-el-files-in-config-org ()
  (let* ((source-dir (expand-file-name "config-org" mcg-emacs-dir))
         (generated-files
          (mapcar (lambda (file) (file-relative-name file source-dir))
                  (directory-files-recursively source-dir "\\.el\\'"))))
    (should (equal nil generated-files))))

;;; run-tests.el ends here
