;;; aero-claude.el --- Launch Claude Code in vterm -*- lexical-binding: t; -*-

;; minimal wrapper that launches claude code in a vterm buffer with correct
;; rendering, using a split-and-close trick to force proper terminal dimensions
;; and a sync-block renderer to eliminate flicker.

;;; Code:

(require 'vterm)
(declare-function project-root "project")

;;; Anti-flicker renderer
;;
;; Claude Code wraps every screen update in DEC synchronized output markers:
;; \e[?2026h (begin) ... \e[?2026l (end). Terminals that support this render
;; the block atomically. libvterm ignores these markers, so every intermediate
;; draw state (clear screen, reposition, repaint) is visible as flicker.
;;
;; This filter intercepts vterm output, detects sync blocks, and accumulates
;; chunks until the closing marker arrives, then flushes everything to vterm
;; in one shot with inhibit-redisplay bound to t.

(defconst aero/claude--sync-start "\033[?2026h")
(defconst aero/claude--sync-end "\033[?2026l")

(defvar-local aero/claude--in-sync-block nil
  "Non-nil when accumulating a synchronized output block.")

(defvar-local aero/claude--sync-queue nil
  "Accumulated output chunks within a sync block.")

(defvar-local aero/claude--sync-timer nil
  "Safety timer to flush sync queue if end marker never arrives.")

(defvar-local aero/claude--saved-cursor-type nil
  "Saved cursor-type before entering vterm-copy-mode.")

(defvar-local aero/claude--drop-next-render nil
  "When non-nil, discard the next complete sync block instead of flushing.")

(defun aero/claude--buffer-p (buf)
  "Return non-nil if BUF is a claude-code buffer we manage."
  (string-match-p "\\*claude-code\\[" (buffer-name buf)))

(defun aero/claude--flush-sync-queue (buf orig-fun)
  "Flush accumulated sync queue for BUF using ORIG-FUN.
When `aero/claude--drop-next-render' is set, discard the block silently
instead of sending it to vterm, then clear the flag."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when aero/claude--sync-queue
        (when aero/claude--sync-timer
          (cancel-timer aero/claude--sync-timer)
          (setq aero/claude--sync-timer nil))
        (if aero/claude--drop-next-render
            (setq aero/claude--sync-queue nil
                  aero/claude--in-sync-block nil
                  aero/claude--drop-next-render nil)
          (let* ((inhibit-redisplay t)
                 (data (apply #'concat (reverse aero/claude--sync-queue)))
                 (proc (get-buffer-process buf)))
            (setq aero/claude--sync-queue nil
                  aero/claude--in-sync-block nil)
            (when (and proc (process-live-p proc))
              (funcall orig-fun proc data))))))))


(defun aero/claude--smart-renderer (orig-fun process input)
  "Advice around `vterm--filter' that batches synchronized output blocks.
ORIG-FUN is the original filter. PROCESS is the vterm process. INPUT
is the terminal output chunk."
  (let ((buf (process-buffer process)))
    (if (or (not buf) (not (buffer-live-p buf))
            (not (aero/claude--buffer-p buf))
            (not (stringp input)))
        (funcall orig-fun process input)
      (with-current-buffer buf
        (cond
         ;; currently accumulating a sync block
         (aero/claude--in-sync-block
          (push input aero/claude--sync-queue)
          (when (string-search aero/claude--sync-end input)
            (aero/claude--flush-sync-queue buf orig-fun)))

         ;; new sync block starts in this chunk
         ((string-search aero/claude--sync-start input)
          (setq aero/claude--in-sync-block t)
          (push input aero/claude--sync-queue)
          ;; if end marker is in the same chunk, flush immediately
          (if (string-search aero/claude--sync-end input)
              (aero/claude--flush-sync-queue buf orig-fun)
            ;; safety timeout: flush after 50ms if end marker never arrives
            (when aero/claude--sync-timer
              (cancel-timer aero/claude--sync-timer))
            (setq aero/claude--sync-timer
                  (run-at-time 0.05 nil
                               #'aero/claude--flush-sync-queue
                               buf orig-fun))))

         ;; normal output outside sync blocks — pass through
         (t (funcall orig-fun process input)))))))

(defun aero/claude--copy-mode-hook ()
  "Toggle cursor visibility for vterm-copy-mode."
  (if vterm-copy-mode
      (progn
        (setq aero/claude--saved-cursor-type cursor-type)
        (when (null cursor-type)
          (setq cursor-type t)))
    (setq cursor-type aero/claude--saved-cursor-type)))

;;; Buffer configuration

(defun aero/claude--configure-buffer ()
  "Apply buffer-local vterm tuning for Claude Code."
  (setq-local vterm--redraw-immediately nil)
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local cursor-type nil)
  (hl-line-mode -1)
  (make-local-variable 'global-hl-line-mode)
  (setq global-hl-line-mode nil)
  (face-remap-add-relative 'nobreak-space :inherit 'default)
  (add-hook 'vterm-copy-mode-hook #'aero/claude--copy-mode-hook nil t)
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc t)))

;;; Startup resize

(defun aero/claude--force-resize ()
  "Force vterm to recalculate dimensions via split-and-close trick.
Sets `aero/claude--drop-next-render' so the sync renderer silently
discards the intermediate-size render from the split, then closes
the split to trigger the final correct-size render."
  (when-let ((win (get-buffer-window (current-buffer))))
    (let ((buf (current-buffer)))
      (setq aero/claude--drop-next-render t)
      (let ((split (split-window win -5 'below)))
        (run-with-timer 0.3 nil
                        (lambda (w b)
                          ;; clear drop flag before close in case intermediate
                          ;; render never arrived as a sync block
                          (when (buffer-live-p b)
                            (with-current-buffer b
                              (setq aero/claude--drop-next-render nil)))
                          (when (window-live-p w)
                            (delete-window w))
                          (run-with-timer 0.5 nil
                                          (lambda (b)
                                            (when (buffer-live-p b)
                                              (with-current-buffer b
                                                (vterm-clear-scrollback))))
                                          b))
                        split buf)))))

(defun aero/claude--poll-for-startup (buffer retries)
  "Poll BUFFER for Claude Code startup text, trigger resize when found.
Checks every 0.5s for up to RETRIES attempts."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (if (string-match-p "claude" content)
            (aero/claude--force-resize)
          (when (> retries 0)
            (run-with-timer 0.5 nil
                            #'aero/claude--poll-for-startup
                            buffer (1- retries))))))))

;;; Navigation

(defun aero/claude-jump-to-prompt ()
  "Jump to the end of the buffer and recenter so the last line is visible."
  (interactive)
  (goto-char (point-max))
  (recenter -1))

;;; Clipboard image paste

(defun aero/claude-paste-image ()
  "Save clipboard image to a temp file and insert its path into vterm.
Uses pngpaste on macOS to extract the image from the system clipboard."
  (interactive)
  (let ((tmpfile (make-temp-file "claude-img-" nil ".png")))
    (if (not (zerop (call-process "pngpaste" nil nil nil tmpfile)))
        (progn
          (delete-file tmpfile)
          (message "No image in clipboard"))
      (vterm-send-string tmpfile)
      (message "Pasted image: %s" tmpfile))))

;;; Entry point

;;;###autoload
(defun aero/claude ()
  "Launch Claude Code in a vterm buffer for the current project."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj
                   (project-root proj)
                 default-directory))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (buf-name (generate-new-buffer-name
                    (format "*claude-code[%s]*" project-name)))
         (default-directory root)
         (vterm-shell "claude")
         (vterm-buffer-name buf-name))
    (vterm buf-name)
    (aero/claude--configure-buffer)
    (local-set-key (kbd "C-<escape>") #'vterm-send-escape)
    (local-set-key (kbd "C-S-v") #'aero/claude-paste-image)
    (evil-local-set-key 'normal (kbd "G") #'aero/claude-jump-to-prompt)
    (run-with-timer 1.0 nil
                    #'aero/claude--poll-for-startup
                    (current-buffer) 15)))

;; install advice once at load time
(advice-add 'vterm--filter :around #'aero/claude--smart-renderer)

(provide 'aero-claude)

;;; aero-claude.el ends here
