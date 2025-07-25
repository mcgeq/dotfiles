;;; init-builtin.el -- Config for Builtin -*- lexical-binding: t; -*-

;; Filename: init-builtin.el
;; Description: Config for Builtin
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Builtin
;;

;;; Installation:
;;
;; Put init-builtin.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-builtin)
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
(require 'savehist)
(require 'saveplace)
(require 'whitespace)
(require 'autorevert)
(require 'hideshow)
(require 'newcomment)

;; savehist
(add-hook 'after-init-hook 'savehist-mode)

(setq enable-recursive-minibuffers t)
(setq history-length 1000)
(setq savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history))
(setq savehist-autosave-interval 300)

;; saveplacs
(add-hook 'after-init-hook 'save-place-mode)

;;; Code:

;; whitespace
(add-hook 'after-init-hook 'global-whitespace-mode)
(face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
(face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))
(face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))
(setq
whitespace-line-column nil
whitespace-style
'(face
  empty
  lines-tail
  space-before-tab
  trailing
  tabs
  tab-mark
)
)

;; autorevert
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; hideshow
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; 这里额外启用了 :box t 属性使得提示更加明显
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

(defun hideshow-folded-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... #%d " nlines)))
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay 'hideshow-folded-overlay-fn)

;; newcomment
;; 2. 绑定键位
(global-set-key [remap comment-dwim] #'comment-or-uncomment)

;; 3. 定义 comment-or-uncomment 函数
(defun comment-or-uncomment ()
  "Toggle comment on the selected region or the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

;; 4. 设置自定义变量
(setq comment-auto-fill-only-comments t)

(provide 'init-builtin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-builtin.el ends here
