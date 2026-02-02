;;; early-init.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019-2025 Jade Michael Thornton
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;;; Commentary:
;;
;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.
;;
;;; Code:

;; LSP_USE_PLISTS must be set before Emacs loads packages (affects JSON parsing optimization)
(setenv "LSP_USE_PLISTS" "true")

;; Use most recent byte-compiled files
(setq load-prefer-newer t)

;; Native-compile async
(setq native-comp-jit-compilation t)

;; exclude test files from native compilation to prevent stuck compilation processes
(setq native-comp-jit-compilation-deny-list '(".*-tests?\\.el$"))

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Package initialize occurs automatically, before `user-init-file' is loaded, but after
;; `early-init-file'. We want Aero to handle this, so stop Emacs from doing it on its own.
(setq package-enable-at-startup nil)

;; Always use utf-8 for everything, I'll change it on the fly if I need something else for some
;; reason.
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Disable useless UI features by default.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Disable mail accidentally opening
(global-unset-key (kbd "C-x m"))
(defun goto-address-find-address-at-point () "Disabled by Aero." nil)

;; Mark the auto-tangle eval form as safe so we don't get prompted on every startup
(setq safe-local-eval-forms
      '((add-hook 'after-save-hook #'org-babel-tangle nil t)))

(setq site-run-file nil ; One less file to load at startup, and we'll never use it
      frame-inhibit-implied-resize t ; only explicit resize
      window-resize-pixelwise t
      frame-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      initial-buffer-choice t ; use scratch buffer
      initial-major-mode 'fundamental-mode)
