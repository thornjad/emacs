;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2023 Jade Michael Thornton
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
;;; Commentary:
;;
;; MacOS-specific clipboard interface functionality
;;
;;; Code:

(defvar aero/pbcopier-program (executable-find "pbcopy")
  "Name of Pbcopy program tool.")
(defvar pbpaste-program (executable-find "pbpaste")
  "Name of Pbpaste program tool.")

(defvar aero/pbcopier-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.")

(defvar aero/pbcopier-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from pbcopy.")

(defvar aero/pbcopier-last-selected-text-primary nil
  "The value of the PRIMARY X selection from pbcopy.")

(defun aero/pbcopier-set-selection (type data)
  "TYPE is a symbol: primary, secondary and clipboard.
See `x-set-selection'."
  (when aero/pbcopier-program
    (let* ((process-connection-type nil)
           (proc (start-process "pbcopy" nil "pbcopy"
                                "-selection" (symbol-name type))))
      (process-send-string proc data)
      (process-send-eof proc))))

(defun aero/pbcopier-select-text (text)
  "See `x-select-text'."
  (aero/pbcopier-set-selection 'primary text)
  (setq aero/pbcopier-last-selected-text-primary text)
  (when aero/pbcopier-select-enable-clipboard
    (aero/pbcopier-set-selection 'clipboard text)
    (setq aero/pbcopier-last-selected-text-clipboard text)))

(defun aero/pbcopier-selection-value ()
  "See `x-cut-buffer-or-selection-value'."
  (when aero/pbcopier-program
    (let (clip-text primary-text)
      (when aero/pbcopier-select-enable-clipboard
        (let ((tramp-mode nil)
              (default-directory "~"))
          (setq clip-text (shell-command-to-string "pbpaste")))
        (setq clip-text
              (cond ;; check clipboard selection
               ((or (not clip-text) (string= clip-text ""))
                (setq aero/pbcopier-last-selected-text-primary nil))
               ((eq      clip-text aero/pbcopier-last-selected-text-clipboard) nil)
               ((string= clip-text aero/pbcopier-last-selected-text-clipboard)
                ;; Record the newer string,
                ;; so subsequent calls can use the `eq' test.
                (setq aero/pbcopier-last-selected-text-clipboard clip-text)
                nil)
               (t (setq aero/pbcopier-last-selected-text-clipboard clip-text)))))
      (let ((tramp-mode nil)
            (default-directory "~"))
        (setq primary-text (shell-command-to-string "pbpaste")))
      (setq primary-text
            (cond ;; check primary selection
             ((or (not primary-text) (string= primary-text ""))
              (setq aero/pbcopier-last-selected-text-primary nil))
             ((eq      primary-text aero/pbcopier-last-selected-text-primary) nil)
             ((string= primary-text aero/pbcopier-last-selected-text-primary)
              ;; Record the newer string,
              ;; so subsequent calls can use the `eq' test.
              (setq aero/pbcopier-last-selected-text-primary primary-text)
              nil)
             (t (setq aero/pbcopier-last-selected-text-primary primary-text))))
      (or clip-text primary-text))))

(provide 'aero-pbcopier)
