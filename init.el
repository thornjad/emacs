;;; init.el --- Thornjad's Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2016-2025 Jade Michael Thornton
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
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;; Thornjad's primary init file, containing important setup and calling org-babel on the main
;; literate config.
;;
;;; Code:

;; Catch-all version check, should be updated when we use a new feature that's not
;; backward-compatible.
(when (and (not (member "--no-version-check" command-line-args))
           (version< emacs-version "28"))
  (error "Thornjad Emacs requires at least Emacs version 28. Please upgrade or use --no-version-check"))

(defvar aero/gc-cons '((400000000 1.0) (100000000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high value to avoid collection,
temporarily trading space for cycles, but not so high that we require OS paging. During normal
execution, the normal value (cadr) is used, a bit above the default of 800 KiB, to reverse the trade
so we use more cycles but less space, but not too little space.")


;; Expand GC parameters so we use fewer, larger collections instead of more, smaller ones. On
;; modern systems with plenty of RAM, this should speed up Emacs slightly, at least in theory.
;; This is controversial, but I figure it makes enough sense to keep in here.
(eval-when-compile (defvar aero/gc-cons))
(setq gc-cons-threshold (car (cadr aero/gc-cons))
      gc-cons-percentage (cadr (cadr aero/gc-cons)))
(setq read-process-output-max #x1000000)

;; But avoid garbage collection during startup by increasing thresholds.
(let ((gc-cons-threshold (car (car aero/gc-cons)))
      (gc-cons-percentage (cadr (car aero/gc-cons)))
      (debug-on-error t))

  ;; BUT, trade memory for fewer cycles when using the minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold (car (car aero/gc-cons)))))
  (add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold (car (cadr aero/gc-cons)))))

	;; NOTE to future self, Doom has an optimization where `file-name-handler-alist' is set to nil
	;; during startup because many IO functions consult it needlessly. However, this stops Emacs from
	;; falling back to *.el.gz files if it can't find native- or byte-compiled versions of a package.
	;; This breaks often enough that it's not worth it to copy this behavior.

  (setq load-prefer-newer t) ; Load newest code during init
  (prefer-coding-system 'utf-8) ; Just in case early-init missed this one, or old Emacs
  (setq ad-redefinition-action 'accept) ; Accept advice redefinition without complaining

  ;; Ensure this init file is the one we can reference later
  (setq user-init-file (or load-file-name (buffer-file-name)))

  ;; Had a problem once with user-emacs-directory, so set it for sure here
  (setq user-emacs-directory (file-name-directory user-init-file))

  ;; burn baby burn
  (require 'org)
  (require 'ob-tangle)
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory)))

;;; init.el ends here
