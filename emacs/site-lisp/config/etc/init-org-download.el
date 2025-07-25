;;; init-org-download.el -- Config for Org Download -*- lexical-binding: t; -*-

;; Filename: init-org-download.el
;; Description: Config for Org Download
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-18 Sat 15:57>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Org Download
;;

;;; Installation:
;;
;; Put init-org-download.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org-download)
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
(require 'org-download)

;;; Code:

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(add-hook 'org-mode-hook 'org-download-enable)

(setq org-download-display-inline-images 'posframe)

(defun mg/org-insert-clipboard-image (width)
    "create a time stamped unique-named file from the clipboard in the sub-directory
 (%filename.assets) as the org-buffer and insert a link to this file."
    (interactive (list
                  (read-string (format "Input image width, default is 800: ")
                               nil nil "800")))
    ;; 设置图片存放的文件夹位置为 `当前 Org 文件同名.assets'
    (setq foldername (concat (file-name-base (buffer-file-name)) ".assets/"))
    (if (not (file-exists-p foldername))
        (mkdir foldername))
    ;; 设置图片的文件名，格式为 `img_年月日_时分秒.png'
    (setq imgName (concat "img_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
    ;; 图片文件的相对路径
    (setq relativeFilename (concat (file-name-base (buffer-name)) ".assets/" imgName))
    ;; 根据不同的操作系统设置不同的命令行工具
    (when *is-win32p*
          (setq org-download-screenshot-method (concat "magick clipboard: " relativeFilename)))

        (when *is-linux*
          (setq org-download-screenshot-method ("xclip -selection clipboard -t image/png -o > " relativeFilename)))

        (when *is-mac*
          (setq org-download-screenshot-method ("pngpaste " relativeFilename)))
    ;; 给粘贴好的图片链接加上宽度属性，方便导出
    (insert (concat "\n#+DOWNLOADED: screenshot @ "
                    (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time))
                    "\n#+CAPTION: \n#+ATTR_ORG: :width "
                    width
                    "\n#+ATTR_LATEX: :width "
                    (if (>= (/ (string-to-number width) 800.0) 1.0)
                        "1.0"
                      (number-to-string (/ (string-to-number width) 800.0)))
                    "\\linewidth :float nil\n"
                    "#+ATTR_HTML: :width "
                    width
                    "\n[[file:" relativeFilename "]]\n"))
    (org-download-screenshot)
    )

(provide 'init-org-download)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-download.el ends here
