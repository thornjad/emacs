;;; wttrin.el --- Emacs frontend for weather web service wttr.in

;; Copyright (C) 2023 Jade Michael Thornton
;; Copyright (C) 2016 Carl X. Su
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
;; Provides the weather information from wttr.in based on your query condition.

(require 'url)

;;; Code:

(defgroup wttrin nil
  "Emacs frontend for weather web service wttr.in."
  :prefix "wttrin-"
  :group 'comm)

(defcustom wttrin-default-cities '("55123" "Chicago" "New York City")
  "Specify default cities list for quick completion."
  :group 'wttrin
  :type 'list)

(defcustom wttrin-default-accept-language '("Accept-Language" . "en-US,en;")
  "Specify default HTTP request Header for Accept-Language."
  :group 'wttrin
  :type '(list))

(defun wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://wttr.in/" query "?A&n&F&2&u&T")
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

(defun wttrin-exit ()
  (interactive)
  (quit-window t))

(defun wttrin-query (city-name)
  "Query weather of CITY-NAME via wttrin, and display the result in new buffer."
  (let ((raw-string (wttrin-fetch-raw-string city-name)))
    (if (string-match "ERROR" raw-string)
        (message "Cannot get weather data. Maybe you inputed a wrong city name?")
      (let ((buffer (get-buffer-create (format "*wttr.in - %s*" city-name))))
        (switch-to-buffer buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert raw-string)
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point-min) (1+ (point)))
        (use-local-map (make-sparse-keymap))
        (local-set-key "q" 'wttrin-exit)
        (local-set-key "g" 'wttrin)
        (setq buffer-read-only t)))))

;;;###autoload
(defun wttrin (city)
  "Display weather information for CITY."
  (interactive
   (list (completing-read "City name: " wttrin-default-cities nil nil
                          (when (= (length wttrin-default-cities) 1)
                            (car wttrin-default-cities)))))
  (wttrin-query city))

(provide 'wttrin)

;;; wttrin.el ends here
