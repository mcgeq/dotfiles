;;; init.el -- Config for Capture Template -*- lexical-binding: t; -*-

;; Filename: init-capture-hugo.el
;; Description: Config for Capture Template
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:39>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Capture Template
;;

;;; Installation:
;;
;; Put init-capture-hugo.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-capture-hugo)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-aider RET
;;

;;; Change log:
;;

;;; Require:
(require 'ox)
(require 'ox-hugo)
(require 'easy-hugo)
(require 'org-capture)

;;; Code:
(setq org-hugo-base-dir (concat mcgemacs-blog-dir "/"))
(add-hook 'org-capture-mode-hook #'(lambda() (setq-local
                                              org-complete-tags-always-offer-all-agenda-tags t)))
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(setq org-capture-use-agenda-date nil)
;; define common template

(setq org-capture-templates nil)
(add-to-list 'org-capture-templates '("t" "Tasks"))
(add-to-list 'org-capture-templates '("tr" "Book Reading Task" entry (file+olp org-file-task "Reading Book")
                                       "* TODO %^{书名}\n%u\n%a\n" :clock-in t))
(add-to-list 'org-capture-templates '("tw" "Work Task" entry (file+headline org-file-task "Work")
                                       "* TODO %^{任务名}\n%u\n%a\n" :clock-in t :clock-resume t))

(add-to-list 'org-capture-templates '("d" "Diary"))
(add-to-list 'org-capture-templates '("dt" "Today's TODO List" entry (file+olp+datetree org-file-diary)
                                      "* Today's TODO List [/]\n%T\n\n** TODO %?"
                                      :empty-lines 1
                                      :jump-to-captured t))
(add-to-list 'org-capture-templates '("do" "Other stuff" entry (file+olp+datetree org-file-diary)
                               "* %?\nT\n\n%i"
                               :empty-lines 1
                               :jump-to-captured t))

(add-to-list 'org-capture-templates '("n" "Notes" entry (file org-file-note)
                                       "* %^{heading} %t %^g\n %?\n"
                                       :empty-lines 1))

(add-to-list 'org-capture-templates '("j" "Journal" entry (file+datetree org-file-journal)
                                       "* %U - %^{heading} %^g\n %?\n"
                                       :empty-lines 1))

(add-to-list 'org-capture-templates '("b" "Billing" plain (file+function org-file-billing find-month-tree)
                                      " | %U | %^{类别} | %^{描述} | %^{金额} |" :kill-buffer t))

(add-to-list 'org-capture-templates '("p" "Passwords" entry (file org-file-password)
                                      "* %U - %^{title} %^G\n\n - 用户名: %^{用户名}\n - 密码: %(get-or-create-password)"
                                      :empty-lines 1
                                      :kill-buffer t))
;; 管理密码 https://www.zmonster.me/2018/02/28/org-mode-capture.html#org17ea029

;; 加密密码
(defun random-alphanum ()
  (let* ((charset "abcdefghijklmnopqurstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (x (random 62)))
    (char-to-string (elt charset x))))

(defun create-password ()
  (let ((value ""))
    (dotimes (number 16 value)
      (setq value (concat value (random-alphanum))))))

(defun get-or-create-password ()
  (setq password (read-string "Password: "))
  (if (string= password "")
      (create-password)
    password))

(defun get-year-and-month ()
  (list (format-time-string "%Y 年") (format-time-string "%m 月")))

(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
            (goto-char (line-beginning-position))
          (progn
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
  See `org-capture-templates' for more information."
    (let* (
           (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ;;,(concat ":EXPORT_FILE_NAME: " "index")
                   ,(concat ":EXPORT_DATE: " date)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))


  (add-to-list 'org-capture-templates '("h" "Hugo Post"))
  (add-to-list 'org-capture-templates '("hl" "About Development Language"))
  (add-to-list 'org-capture-templates
               '("hla"
                 "About ArkTS"
                 entry
                 (file+headline org-development-blog "ArkTS")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hle"
                 "About React"
                 entry
                 (file+headline org-development-blog "React")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hlm"
                 "About Assembly"
                 entry
                 (file+headline org-development-blog "Assembly")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hlr"
                 "About Rust"
                 entry
                 (file+headline org-development-blog "Rust")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hlc"
                 "About C#"
                 entry
                 (file+headline org-development-blog "CSharp")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hlv"
                 "About Vue"
                 entry
                 (file+headline org-development-blog "Vue")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hlu"
                 "About MCU"
                 entry
                 (file+headline org-mcu-blog "MCU")
                 (function org-hugo-new-subtree-post-capture-template)))

  (add-to-list 'org-capture-templates
               '("he"                ;`org-capture' binding + h
                 "About Emacs"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp org-emacs-blog "Emacs")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hp"
                 "About Prattle"
                 entry
                 (file+olp org-prattle-blog "Prattle")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
               '("hT"
                 "About Tools"
                 entry
                 (file+olp org-tools-blog "Tools")
                 (function org-hugo-new-subtree-post-capture-template)))
 )

(with-eval-after-load 'easy-hugo
  (setq easy-hugo-postdir "content-org")
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-url "https://www.gemc.club")
  (setq easy-hugo-basedir (concat mcgemacs-blog-dir "/")))


(defun choice-series ()
  "Prompt the user for a choice series and return the input."
  (let ((input (read-from-minibuffer "Choice Series: ")))  ;; 读取输入
    input))  ;; 返回输入

(with-eval-after-load 'easy-hugo
  (setq easy-hugo-postdir "content-org")
  (setq easy-hugo-default-ext ".org")
  (setq easy-hugo-url "https://www.gemc.club")
  (setq easy-hugo-basedir (concat mcgemacs-blog-dir "/")))

(provide 'init-capture-hugo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-capture-hugo.el ends here
