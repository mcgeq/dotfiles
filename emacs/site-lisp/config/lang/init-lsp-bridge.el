;;; init-lsp-bridge.el -- Config for Lsp Bridge -*- lexical-binding: t; -*-

;; Filename: init-lsp-bridge.el
;; Description: Config for Lsp Bridge
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcgeq <mcgeq@outlook.com>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Lsp Bridge
;;

;;; Installation:
;;
;; Put init-lsp-bridge.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-lsp-bridge)
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

(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)
(require 'lang-cpp)
;;(require 'lang-java)
;;(require 'lang-typescript)
(require 'lang-rust)
;;(require 'lang-css-mode)
;;(require 'lang-web-mode)
;;(require 'lang-clojure)
;; (require 'lang-lua)

(unless (display-graphic-p)
  (require 'acm-terminal))

;;; Code:
;; custom language server dir
(setq lsp-bridge-user-langserver-dir mcgemacs-custom-lsp-bridge-langserver-dir)
(setq lsp-bridge-user-multiserver-dir mcgemacs-custom-lsp-bridge-multiserver-dir)
(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-enable-org-babel t)
(setq lsp-bridge-enable-inlay-hint t)
(setq lsp-bridge-semantic-tokens t)
;; formatter
(setq lsp-bridge-enable-auto-format-code t)
(setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["variable"])

(setq acm-enable-capf t)
(setq acm-enable-quick-access t)
(setq acm-backend-yas-match-by-trigger-keyword t)
(setq acm-enable-tabnine nil)
(setq acm-candidate-match-function 'orderless-flex)
(setq acm-enable-codeium nil)
(setq acm-enable-lsp-workspace-symbol t)

;; (setq lsp-bridge-enable-debug t)

(global-lsp-bridge-mode)

(add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
(add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

(setq lsp-bridge-get-multi-lang-server-by-project
      (lambda (project-path filepath)
        ;;
        (save-excursion
          (when (string-equal (file-name-extension filepath) "ts")
            (dolist (buf (buffer-list))
              (when (string-equal (buffer-file-name buf) filepath)
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (search-backward-regexp (regexp-quote "from \"https://deno.land") nil t)
                    (return "deno")))))))))

(lazy-load-set-keys
 '(
   ("C-c C-u d l" .  lsp-bridge-diagnostic-list)
   ("C-c C-u d w" .  lsp-bridge-workspace-diagnostic-list)
   )
 lsp-bridge-mode-map)

;; save file update time
;; 在编程模式下启用保存文件时自动更新 `Last Modified` 时间的功能
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'mcge/update-file-headers nil t)))

(provide 'init-lsp-bridge)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
