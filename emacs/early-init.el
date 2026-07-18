;;; early-init.el --- Early Init for MCG Emacs -*- lexical-binding: t; -*-

;; Filename: early-init.el
;; Description: Early initialization before GUI
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024-2026, mcge, all rights reserved.
;; Create   Date: 2026-01-08
;; Version: 3.0
;; Keywords: early-init, frame, startup, performance
;; Compatibility: GNU Emacs 29.0+

;;; Commentary:
;;
;; This file is loaded before the GUI is initialized.
;; Settings here take effect before the first frame is created.
;;
;; Includes:
;; - Package initialization control
;; - GC and performance optimization
;; - Frame size and UI elements
;; - Native compilation settings
;;
;; Copy generated early-init.el to ~/.emacs.d/early-init.el
;;

;;; Performance Optimization

;; Save original values for restoration after startup
(defvar mcg--file-name-handler-alist-original file-name-handler-alist
  "Original file-name-handler-alist.")

;; Disable GC and file-name-handler during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Keep redisplay enabled so Windows can paint the configured dark initial
;; frame immediately instead of showing an unpainted white window.

(defvar mcg--startup-redisplay-released nil
  "Non-nil once startup redisplay inhibition has been released.")

(defun mcg-release-startup-redisplay ()
  "Release startup redisplay inhibition once."
  (unless mcg--startup-redisplay-released
    (setq inhibit-redisplay nil
          inhibit-message nil
          mcg--startup-redisplay-released t)
    (redisplay)))

(add-hook 'window-setup-hook #'mcg-release-startup-redisplay)

;;; Native Compilation

(defvar mcg-early-init-eln-cache-dir nil
  "Native-comp ELN cache directory selected during early-init.")

(defun mcg--early-init-emacs-dir ()
  "Return the MCG Emacs root directory available during early-init."
  (let* ((init-file (or user-init-file
                        (expand-file-name "init.el" user-emacs-directory)))
         (init-dir (and init-file (file-name-directory init-file)))
         (init-dir-name (and init-dir
                             (file-name-nondirectory
                              (directory-file-name init-dir))))
         (parent-dir (and init-dir
                          (file-name-directory
                           (directory-file-name init-dir))))
         (parent-dir-name (and parent-dir
                               (file-name-nondirectory
                                (directory-file-name parent-dir)))))
    (file-name-as-directory
     (expand-file-name
      (cond
       ((and init-dir
             (file-exists-p (expand-file-name "site-lisp/config/init.el"
                                              init-dir)))
        init-dir)
       ((and (string= init-dir-name "config")
             (string= parent-dir-name "site-lisp"))
        (file-name-directory (directory-file-name parent-dir)))
       ((and user-emacs-directory
             (file-exists-p (expand-file-name "site-lisp/config/init.el"
                                              user-emacs-directory)))
        user-emacs-directory)
       (t user-emacs-directory))))))

(when (boundp 'native-comp-eln-load-path)
  (setq mcg-early-init-eln-cache-dir
        (expand-file-name ".cache/eln-cache/" (mcg--early-init-emacs-dir)))
  (unless (file-directory-p mcg-early-init-eln-cache-dir)
    (make-directory mcg-early-init-eln-cache-dir t))
  (if (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache mcg-early-init-eln-cache-dir)
    (setq native-comp-eln-load-path
          (cons mcg-early-init-eln-cache-dir
                (remove mcg-early-init-eln-cache-dir
                        native-comp-eln-load-path)))))

(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)
  ;; Set compilation speed (2 = maximum optimization)
  (setq native-comp-speed 2))

;; Prefer loading newer files
(setq load-prefer-newer t)

;;; Frame Configuration

(defvar mcg-frame-width 140
  "Default frame width in characters.")

(defvar mcg-frame-height 56
  "Default frame height in lines.")

(defvar mcg-initial-frame-background "#232025"
  "Initial frame background color used before the theme is loaded.")

(defvar mcg-initial-frame-foreground "#efd5c5"
  "Initial frame foreground color used before the theme is loaded.")

;; Set frame size and disable UI elements for initial and default frames
(setq default-frame-alist
      `((width . ,mcg-frame-width)
        (height . ,mcg-frame-height)
        (background-color . ,mcg-initial-frame-background)
        (foreground-color . ,mcg-initial-frame-foreground)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

(setq initial-frame-alist default-frame-alist)

;; Disable UI elements (must set variables before modes are loaded)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Frame resize behavior
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;;; Startup Screen

;; Disable startup screen and messages
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

;;; Windows Encoding

(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq process-coding-system-alist
        (append '(("grep" . (utf-8 . utf-8))
                  ("rg" . (utf-8 . utf-8)))
                process-coding-system-alist)))

;;; early-init.el ends here
