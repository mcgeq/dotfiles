;;; init-function.el -- Config for Functions -*- lexical-binding: t; -*-

;; Filename: init-function.el
;; Description: Config for Functions
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcgeq <mcgeq@outlook.com>
;; Last Modified:  <2025-02-16 Sun 11:46>
;; Keywords:
;; Compatibility: 31.0.50

;;; Commentary:
;;
;; Config for Functions
;;

;;; Installation:
;;
;; Put init-function.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-function)
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

;;; Code:

(defun mcg/open-init-file()
  "Quick open configuration file."
  (interactive)
  (if (file-exists-p (concat mcgemacs-config-dir "/init.org"))
      (find-file (concat mcgemacs-config-dir "/init.org"))
    (message "Please chek the file does not exist.")
    )
  )

;; 将列表加入到列表的函数
(defun add-list-to-list (dst src)
  "Similar to `add-to-list', DST SRC but accepts a list as 2nd argument."
  (set dst
       (append (eval dst) src)))

(defun +vc-branch-name ()
  (when vc-mode
    (propertize
     (replace-regexp-in-string
      "Git[-:]"
      ""
      (substring-no-properties vc-mode))
     'face
     'bold)))

(defun mcg/customize-image-file-name ()
  "Define the image save directory and name based on the current file name."
  (defvar full-file-name)
  (setq full-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (let* ((imagefile "./images/"))
    (unless (file-exists-p imagefile)
      (make-directory imagefile))
    (setq imagefile (concat "./images/" full-file-name "/"))
    ;;    (setq imagefile (concat "./images/" (format-time-string "%Y%m%d") "/"))
    (unless (file-exists-p imagefile)
      (make-directory imagefile))
    (concat (make-temp-name (concat imagefile
                                    (format-time-string "%Y%m%d_%H%M%S_")))
            ".png"))
  )

;; window insert org images
;; `https://emacs-china.org/t/win10-emacs-org-mode/21942/2'
(defun mcg/org-screenshot-on-windows ()
  "Windows platform insert pictures in the org file."
  (interactive)
  (let ((filename (mcg/customize-image-file-name)))
    (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"
                           filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved
as file'} else {Write-Output 'clipboard does not contain image data'}\""))
    (insert (concat "[[file:" filename "]]")))
  (org-display-inline-images)
  )

(defun mcg/open-source-code-dir ()
  "Open source coding directory."
  (interactive)
  )

(defun mcge/update-last-modified-time ()
  "Update the `Last Modified' timestamp in the current buffer."
  (interactive)
  (when (and (buffer-file-name)
             (not (string-match-p "init-function" (buffer-name))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Last Modified:.*$" nil t)
        (replace-match (concat "Last Modified:  " (format-time-string "%Y-%m-%d %H:%M:%S")))))))

;; Update modified By
(defun mcge/update-last-modified-by ()
  "Update the `Modified By' in the current buffer."
  (interactive)
  (when (and (buffer-file-name)
             (not (string-match-p "init-function" (buffer-name))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Modified   By:.*$" nil t)
        (replace-match (concat "Modified   By:  " (get-git-user-name)
                               " <" (get-git-user-email) ">"))))))

(defun mcge/update-file-headers ()
  "Update the `Last Modified' and `Modified By' lines in the current buffer."
  (interactive)
  (mcge/update-last-modified-time)
  (mcge/update-last-modified-by))

(defun mcge-smart-move-beginning-of-line ()
  "Move point to the first non-whitespace character, or to the beginning of the line."
  (interactive)  ;; This makes the function callable as a command
  (let ((current-point (point)))
    (back-to-indentation)  ;; Move to the first non-whitespace character
    (when (eq current-point (point))  ;; If point was already there, move to beginning of line
      (move-beginning-of-line 1))))

;; 获取 Git 用户名和邮箱
(defun get-git-user-info ()
  "返回 Git 配置的用户名和邮箱组成的 cons 对"
  (let ((name (string-trim (shell-command-to-string "git config user.name")))
        (email (string-trim (shell-command-to-string "git config user.email"))))
    (if (or (string-empty-p name) (string-empty-p email))
        (setq name (user-login-name)
              email "mcgeq@outlook.com")) ;; Default email if Git config is missing
    (cons name email)))

;; 生成标准时间戳格式
(defun generate-timestamp ()
  "生成格式为 <YYYY-MM-DD Day HH:MM> 的时间戳"
  (format-time-string "<%Y-%m-%d %a %H:%M>"))

;; 更新文件头元数据
(defun mcg/org-update-file-headers ()
  "自动更新 Modified By 和 Last Modified 字段"
  (interactive)
  (when (and (buffer-file-name)
             (string-match "\\.org\\'" (buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (let* ((git-info (get-git-user-info))
             (user-name (car git-info))
             (user-email (cdr git-info))
             (timestamp (generate-timestamp)))
        ;; 更新 Modified By
        (when (re-search-forward "Modified   By:.*$" nil t)
          (replace-match (format "Modified   By:  %s <%s>" user-name user-email)))
        ;; 更新 Last Modified
        (goto-char (point-min))
        (when (re-search-forward "Last Modified:.*$" nil t)
          (replace-match (format "Last Modified:  %s" timestamp)))
        ;; 更新 Compatibility
        (goto-char (point-min))
        (when (re-search-forward "Compatibility:.*$" nil t)
          (replace-match (format "Compatibility: %s" emacs-version))))
    (message "File header updated"))))

(provide 'init-function)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-function.el ends here
