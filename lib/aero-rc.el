;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2025 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
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
;;; Code:

(require 'aero-lib)


(setq-default
 ;; general
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 use-dialog-box nil ; use minibuffer to ask questions instead
 use-short-answers t ; y-or-n instead of yes-or-no
 sentence-end-double-space nil ; the world will not go to shit today
 default-fill-column 100 ; i am mortal, not Arthur Whitney
 fill-column 100
 help-window-select t ; focus help window when opened
 help-clean-buttons t ; remove quotes from buttons (why isn't this the default)
 kill-ring-max 5000 ; truncate kill ring after 5000 entries
 mark-ring-max 5000 ; truncate mark ring after 5000 entries
 kill-do-not-save-duplicates t ; don't add duplicate strings to kill-ring
 apropos-do-all t ; apropos is apropos
 global-display-line-numbers-mode nil ; fuck line numbers
 gnutls-min-prime-bits 4096 ; 256 est absurde
 confirm-kill-emacs 'yes-or-no-p ; too easy to kill when looking for alt file
 switch-to-buffer-preserve-window-point t ; try to preserve point position in closed buffers
 next-error-message-highlight t
 line-move-visual t ; move lines by display, not reality
 make-pointer-invisible t ; le curseur est une chienne
 auto-revert-interval 10 ; wait just a little longer (default is 5)
 pop-up-windows nil ; make new window for pop-ups
 window-sides-slots '(0 1 1 1) ; side-window slots (left top right bottom)
 shared-game-score-directory (expand-file-name "game-scores/" aero-etc-dir)
 idle-update-delay 0.5 ; default is 0.5
 bidi-paragraph-direction 'left-to-right ; no need to check
 bidi-inhibit-bpa t ; don't look for bidi paren balancing
 create-lockfiles nil ; tries to solve a non-existent problem and causes trouble doing it
 jit-lock-defer-time 0 ; wait to fontify until input ends, but no longer
 ns-use-srgb-colorspace nil ;; REVIEW what is this?
 show-paren-context-when-offscreen t ; for some langs, show context in echo area
 mail-user-agent nil ; disable email click opening mail message; error instead
 context-menu-mode t ; enable context menu when clicked, should be default
 isearch-forward t ; ensures evil repeats searches in the correct direction
 debugger-stack-frame-as-list t ; more readable Elisp stack traces
 enable-recursive-minibuffers t ; allow minibuffer commands in the minibuffer

 ;; Do not allow the cursor in the minibuffer prompt
 minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)

 ;; Emacs should just have code that automatically sets this threshold according to some function
 ;; involving a constant, the current date, and Moore's Law.
 large-file-warning-threshold 500000000

 ;; Defaults:
 ;; '("gnutls-cli --insecure -p %p %h"
 ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
 ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
 tls-program '("gnutls-cli -p %p %h"
               "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

 ;; Scrolling
 ;; Emacs spends too much effort recentering the screen if you scroll the
 ;; cursor more than N lines past window edges (where N is the settings of
 ;; `scroll-conservatively'). This is especially slow in larger files
 ;; during large-scale scrolling commands. If kept over 100, the window is
 ;; never automatically re-centered.
 scroll-conservatively 101
 scroll-margin 3 ; keep 3 lines at top and bottom of buffer when scrolling
 scroll-preserve-screen-position t ; see variable documentation; this is the modern expectation
 mouse-wheel-scroll-amount '(3 ((shift) . 1)) ; make scroll wheel scroll more at a time
 pixel-resolution-fine-flag 1 ; use pixel scrolling
 hscroll-margin 5 ; like scroll-margin but horizontal
 hscroll-step 1 ; on horizontal scroll, scroll by one column at a time
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
 ;; for tall lines. Thanks to Sacha Chua for the time saved!
 auto-window-vscroll nil
 mouse-wheel-progressive-speed nil ; don't accelerate TODO may not want this?
 comint-scroll-to-bottom-on-input t ; insert at bottom
 comint-scroll-to-bottom-on-output nil ; don't scroll on output by default
 comint-input-ignoredups t ; ignore duplicate inputs in history
 comint-prompt-read-only nil ; breaks shell-command sometimes

 compilation-scroll-output t ; scroll with compilation output
 compilation-max-output-line-length nil ; don't collapse long lines in compilation

 eww-search-prefix "https://lite.duckduckgo.com/lite?q=" ; eww search DuckDuckGo
 dictionary-server "dict.org" ; skip trying to search localhost

 ;; simple frame title; I find the default distracting
 frame-title-format '("Emacs — "
                      (:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b"))
                      (:eval (when (and (buffer-modified-p) (not buffer-read-only))
                               " •")))
 ns-use-proxy-icon nil ; remove icon from frame title in NS
 ns-pop-up-frames nil ; open files in same frame from Finder et al

 initial-scratch-message (concat ";; Welcome to GNU Emacs v" emacs-version
                                 "." (number-to-string emacs-build-number)
                                 "\n;;\n"
                                 ";; Go placidly amid the noise and haste,\n"
                                 ";; and remember what peace there may be in silence.\n"
                                 ";;\n"
                                 ";; Booted in " (emacs-init-time "%.3f seconds")
                                 (format " with %d garbage collections" gcs-done)
                                 (when (boundp 'straight--profile-cache)
                                   (format "\n;; Initialized %d packages"
                                           (+ (hash-table-size straight--profile-cache)
                                              (if (bound-and-true-p package-alist)
                                                  (length package-activated-list)
                                                0))))
                                 "\n\n")

 use-package-verbose nil ; ignore verbose output from use-package

 ;; Save backups to system temp (somewhere in /var on MacOS)
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t ; don't clobber symlinks, our file is what we want
 kept-new-versions 6 ; how many backups to keep
 kept-old-versions 2 ; keep first two versions forever
 delete-old-versions t ; delete backups older than `kept-new-versions' except `kept-old-versions'
 version-control t ; use version numbers in backup files

 git-commit-fill-column 72 ; best length in my opinion
 auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
 save-interprogram-paste-before-kill t ; see variable documentation
 diff-switches "-u" ; unified diff by default

 ;; files
 confirm-nonexistent-file-or-buffer nil ; don't ask to create a buffer
 require-final-newline t ; add newline to end of files if there isn't one
 load-prefer-newer t ; load the newer of equivalent el, elc, eln
 completion-ignore-case t ; ignorer la capitalisation
 read-file-name-completion-ignore-case t ; ignorer la capitalisation des fichiers
 delete-auto-save-files t ; auto-delete auto-save auto-files automatically
 vc-follow-symlinks t ; don't ask to follow symlinks

 world-clock-list '(("America/New_York" "Home")
                    ("America/Los_Angeles" "San Francisco")
                    ("Europe/Warsaw" "Poland")
                    ("Europe/Amsterdam" "Netherlands")
                    ("Europe/London" "London")
                    ("America/Chicago" "Texas")
                    ("America/Denver" "Utah")
                    ("America/Denver" "Colorado")
                    ("Pacific/Auckland" "New Zealand")
                    ("Australia/Sydney" "Sydney"))

 world-clock-time-format "%R %Z (%z) %a %d %b "

 ;; xref
 ;; Use separate xref history for each window, allowing independent code navigation
 xref-history-storage #'xref-window-local-history

 ;; indentation
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2
 css-indent-offset 2
 evil-shift-width 2
 js-indent-level 2
 js-switch-indent-offset 2
 js-syntactic-mode-name nil ; just use normal mode name
 js2-basic-offset 2
 typescript-indent-level 2
 python-indent-offset 4 ; 2 would be too much of a hassle
 rust-indent-offset 4
 sgml-basic-offset 2
 sh-basic-offset 2
 tcl-indent-level 2
 tcl-tab-always-indent t
 lua-indent-level 2
 groovy-indent-offset 2
 web-mode-attr-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-markup-indent-offset 2
 tab-stop-list (number-sequence 2 200 2))

;; ensure lang is set properly
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; underscores delineate words
(modify-syntax-entry ?_ "w")

;; make case statements indent properly
(with-eval-after-load 'prog-mode
  (c-set-offset 'case-label '++))

;; Show trailing whitespace in prog modes
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; enable narrow to region without asking
(put 'narrow-to-region 'disabled nil)

;; prevent savehist cpu hogging
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;; Ensure emacsclient frames open with focus
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

;; Enable mouse in TTY
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Disable stupid super keybindings. These are defined in ns-win.el. In my view, super is the domain
;; of the OS and nothing should be bound with it.
(global-unset-key (kbd "s-:"))
(global-unset-key (kbd "s-C"))
(global-unset-key (kbd "s-D"))
(global-unset-key (kbd "s-E"))
(global-unset-key (kbd "s-F"))
(global-unset-key (kbd "s-d"))
(global-unset-key (kbd "s-e"))
(global-unset-key (kbd "s-f"))
(global-unset-key (kbd "s-g"))
(global-unset-key (kbd "s-j"))
(global-unset-key (kbd "s-k"))
(global-unset-key (kbd "s-l"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-o"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-t"))
(global-unset-key (kbd "s-u"))
(global-unset-key (kbd "s-w"))

;; type to get rid of active selection
(delete-selection-mode t)

;; Try to save point position between sessions
(setq save-place-file (expand-file-name "saveplace" aero-etc-dir))
(save-place-mode 1)

;; ensure buffer names are unique when their filenames are the same. The forward option will expand
;; each duplicate buffer name to include their parent directories as far up as needed to make them
;; unique.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun display-startup-echo-area-message ()
  "Override ridiculous built-in crap."
  (message "Aero est prêt"))

(add-to-list 'display-buffer-alist
             ;; Put eshell in bottom side window
             '("e?shell\\*\\(?:<[[:digit:]]+>\\)?\\'"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (window-height . 23)))

;; If we leave a buffer, set its mark as inactive
(transient-mark-mode 1)

;; Word navigation stops inside camelCaseWords and the like
(global-subword-mode 1)

;; Do not allow the cursor in the minibuffer prompt
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(provide 'aero-rc)
