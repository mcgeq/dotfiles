;;; init-font.el -- Config for Emacs Font -*- lexical-binding: t; -*-

;; Filename: init-font.el
;; Description: Config for Emacs Font
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: <2025-01-09 Thu 18:54>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Font
;;

;;; Installation:
;;
;; Put init-font.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-font)
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
(require 'cl-lib)

;;; Code:
(defun mcg/font-setup-common(character font-list &optional scale-factor)
  "Set fonts for multi CHARACTER from FONT-LIST and modify style with SCALE-FACTOR."
  (cl-loop for font in font-list
           when (find-font (font-spec :name font))
           return (if (not character)
                      (set-face-attribute 'default nil
                                          :family font
                                          :height (cond (*is-mac* 150)
                                                        (*is-win32p* 130)
                                                        (*is-linux* 130)))
                    (when scale-factor (setq face-font-rescale-alist `((,font . ,scale-factor))))
                    (set-fontset-font t character (font-spec :family font) nil 'prepend))))

(defun mcg/font-setup (&optional default-fonts unicode-fonts emoji-fonts cjk-fonts)
  "Font setup, with optional DEFAULT-FONTS, UNICODE-FONTS, EMOJI-FONTS, CJK-FONTS."
  (interactive)
  (when (display-graphic-p)
    (mcg/font-setup-common nil      (if default-fonts default-fonts mcg--fonts-default))
    (mcg/font-setup-common 'unicode (if unicode-fonts unicode-fonts mcg--fonts-unicode))
    (mcg/font-setup-common 'emoji   (if emoji-fonts emoji-fonts mcg--fonts-emoji))
    (dolist (charset '(kana han bopomofo cjk-misc))
      (mcg/font-setup-common charset (if cjk-fonts cjk-fonts mcg--fonts-cjk) 1.2))))

;; Get the font set
(mcg/font-setup)
(add-hook 'window-setup-hook #'mcg/font-setup)
(add-hook 'server-after-make-frame-hook #'mcg/font-setup)

(provide 'init-font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-font.el ends here
