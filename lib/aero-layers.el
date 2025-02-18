;; -*- lexical-binding: t -*-
;;
;; Aero main package layers, most general configuration goes here
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
;;; Commentary:
;;
;;; Code:

(require 'aero-prelude)

;; mostly used by Org
(require 'outline)
(require 'dash)
(require 'notifications)

;; Improved version of help buffers
(package! helpful "Wilfred/helpful"
  :commands (helpful-function
             helpful-variable
             helpful-macro
             helpful-key
             helpful-callable)
  :after (evil general)
  :init

  ;; HACK `help-fns--autoloaded-p's signature changed on Emacs 29. This
  ;; suppresses the error until it is addressed upstream. Basically we just
  ;; override the function to ignore the second argument.
  ;; TODO trying this out without the hack, Jan 27, 2025. If you're reading this and thinking wow
  ;; that was a long time ago, rip it out
  ;; (unless (version< emacs-version "29")
  ;;   (advice-add #'help-fns--autoloaded-p :around
  ;;               (lambda (fn sym &rest args)
  ;;                 (apply fn (list sym)))))

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "hdf" 'helpful-function
   "hda" 'helpful-symbol
   "hdv" 'helpful-variable
   "hdm" 'helpful-macro
   "hdk" 'helpful-key
   "hdc" 'helpful-callable)

  :config
  (evil-define-key 'normal helpful-mode-map
    "q" 'kill-current-buffer
    "?" 'describe-mode))


;;; Better writing

;; Mark passive voice, duplicate words and weasel words
(package! writegood-mode (:repo "bnbeckwith/writegood-mode")
  :hook ((text-mode) . writegood-mode))

;; Mark E′ violations
(package! eprime-mode (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
  :after (general)
  ;; :hook text-mode
  :commands (eprime-check-buffer eprime-mode)
  :init
  (aero-leader-def
    "tp" 'eprime-check-buffer
    "tP" 'eprime-mode))


;;; General Tools

;; My pomodoro package
(package! pomp (:host gitlab :repo "thornjad/pomp")
  :after (general evil)
  :commands (pomp)
  :custom
  (pomp-pomodoro-length 55)
  (pomp-short-break-length 10)
  (pomp-long-break-length 15)
  :init
  (aero-leader-def "ap" 'pomp))

(package! restclient :auto
  :after (general)
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'general)
  (aero-mode-leader-def
    :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))

;; startup profiler
(package! esup "jschaf/esup"
  :commands (esup)
  :config
  ;; Work around a bug where esup tries to profile cl-lib and fails by doing some nil checking
  (defun esup-read-results ()
    "Read all `esup-result' objects from `esup-incoming-results-buffer'.

HACKED by Aero to add nil checking."
    (let (results sep-end-point)
      (with-current-buffer (get-buffer esup-incoming-results-buffer)
        (goto-char esup-last-result-start-point)
        (message "at %s" esup-last-result-start-point)
        (unless (eobp)
          (while (setq sep-end-point (esup-next-separator-end-point))
            (when-let ((result (car (esup-read-result (point)))))
              (push result results))
            (setq esup-last-result-start-point sep-end-point)
            (goto-char esup-last-result-start-point))))
      (nreverse results))))

;; Use the bindings below to insert a virtual comment which displays in the buffer but never saves
;; to disk.
(package! virtual-comment "thanhvg/emacs-virtual-comment"
  :hook ((virtual-comment-make-mode . evil-insert-state))
  :after (general evil)
  :commands (virtual-comment-make
             virtual-comment-next
             virtual-comment-previous
             virtual-comment-delete
             virtual-comment-paste
             virtual-comment-show)
  :custom (virtual-comment-face 'virtual-comment-face)
  :init
  ;; Doesn't define its own faces, using a variable instead, so we need to declare it
  (defface virtual-comment-face
    '((t :inherit highlight))
    "Face for virtual comments"
    :group 'virtual-comment)

  (aero-leader-def
    "v" '(:ignore t :wk "virtual comment")
    "vv" 'virtual-comment-make
    "vn" 'virtual-comment-next
    "vp" 'virtual-comment-previous
    "vk" 'virtual-comment-delete
    "vP" 'virtual-comment-paste
    "vs" 'virtual-comment-show))

;; Weather
(require 'wttrin (expand-file-name "lib/localpackages/wttrin.el" user-emacs-directory))


;;; Programming

;; C language
(package! cc-mode :builtin
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . cpp-mode)
         ("\\.hpp\\'" . cpp-mode))
  :preface
  (defun aero/c-mode-common-hook ()
    "Hook to run in all C modes"
    (set (make-local-variable 'parens-require-spaces) nil))
  :hook (c-mode-common . aero/c-mode-common-hook))

(package! markdown-mode "jrblevin/markdown-mode"
  :after (general smartparens)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode))
  :hook (markdown-mode . flyspell-mode)

  :custom
  ;; Fix table to teach it that quotes mean string, regardless of what the dev says
  (markdown-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (markdown-header-scaling t)
  (markdown-display-remote-images t)
  (markdown-header-scaling-values '(1.3 1.2 1.1 1.0 1.0 1.0))
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-fontify-code-blocks-natively t)

  :config
  ;; Don't expand checkboxes
  (sp-local-pair 'gfm-mode "- [ " "]"))

(package! markdown-toc (:repo "ardumont/markdown-toc")
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(package! yaml-mode (:repo "yoshiki/yaml-mode")
  :mode "\\.ya?ml\\'")

(package! aero-yarn-lock :localpackage
  :mode "yarn\\.lock\\'")


;; Web languages

(package! web-mode "fxbois/web-mode"
  :mode "\\.\\(jsp\\|tpl\\|php\\|xml\\|html?\\|erb\\|svg\\|jsx\\|s?css\\)\\'"
  :custom (web-mode-enable-engine-detection t)
  :config
  ;; If we have tree-sitter, prefer tsx-ts-mode (which will also load eglot)
  (unless (treesitterp) (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))))

(package! emmet-mode :auto
  :hook ((web-mode html-mode css-mode scss-mode js-mode) . emmet-mode)
  :init (setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'js-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(package! jest "thornjad/emacs-jest"
  :commands (jest jest-file jest-test)
  :after (general))

;; Expand className prop in JSX
(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))


;; flymake

(package! flymake :builtin
  :after (general)
  :custom
  ;; left-fringe is the default, but we're being explicit because git-gutter also uses left-fringe.
  ;; Usually this works itself out.
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-wrap-around t)
  (flymake-no-changes-timeout 0.6)

  :config
  ;; Buffer diagnostics in bottom window
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics for.*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 23)))

  (aero-leader-def
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-show-buffer-diagnostics))

;; makes flymake appear in popup
(package! flymake-diagnostic-at-point (:repo "meqif/flymake-diagnostic-at-point")
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(package! flymake-eslint "orzechowskid/flymake-eslint"
  :after (eglot)
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (derived-mode-p '(typescript-mode typescript-ts-mode js-mode web-mode))
                         (not (derived-mode-p 'json-mode))
                         (executable-find "eslint"))
                (flymake-eslint-enable)))))

(package! flyspell :builtin
  :after (general)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config
  (defvar aero-etc-dir)
  (setq
   flyspell-issue-message-flag nil
   ispell-personal-dictionary (expand-file-name "ispell/personal_dictionary.aws" aero/thornlog-path)
   flyspell-sort-corrections nil)

  ;; Skip code inside org src blocks
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (aero-leader-def
    "psP" 'flyspell-prog-mode
    "psN" 'flyspell-goto-next-error
    "psw" 'flyspell-word
    "psb" 'flyspell-buffer
    "psr" 'flyspell-region))

(package! flyspell-lazy (:repo "rolandwalker/flyspell-lazy")
  :hook ((flyspell-mode . flyspell-lazy-mode)))

(package! flyspell-correct "d12frosted/flyspell-correct"
  :after (general flyspell)
  :config
  (aero-leader-def
    "psc" 'flyspell-correct-wrapper
    "psC" 'flyspell-correct-at-point
    "psp" 'flyspell-correct-previous
    "psn" 'flyspell-correct-next))


;; Python

(package! python-mode :auto
  :after (general)
  :mode "\\.py\\'"
  :custom
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  :init
  (setq-default python-shell-interpreter "python3")
  (setq-default python-indent-offset 4)

  :config
  (defvar python-mode-initialized nil)

  (aero-mode-leader-def
    :keymaps 'python-mode-map
    "p" 'run-python
    "s" '(:ignore t :wk "shell send")
    "sd" 'elpy-shell-send-defun
    "sD" 'elpy-shell-send-defun-and-go
    "sb" 'elpy-shell-send-buffer
    "sB" 'elpy-shell-send-buffer-and-go
    "sr" 'elpy-shell-send-region-or-buffer
    "sR" 'elpy-shell-send-region-or-buffer-and-go
    "sc" 'elpy-shell-send-defclass
    "sC" 'elpy-shell-send-defclass-and-go
    "e" '(:ignore t :wk "change executable")
    "ey" 'elpy-switch-to-pypy
    "ec" 'elpy-switch-to-cpython
    "ei" 'elpy-switch-to-ipithon
    "ej" 'elpy-switch-to-jupyter
    "k" '(:ignore t :wk "kill")
    "kk" 'elpy-shell-kill
    "kA" 'elpy-shell-kill-all
    "g" '(:ignore t :wk "go")
    "ge" 'elpy-shell-goto-last-error
    "gi" 'elpy-shell-goto-import-header
    "d" '(:ignore t :wk "pdb")
    "di" '(aero/insert-pdb :wk "insert pdb"))
  (general-define-key
   :keymaps 'python-mode-map
   "s-e" 'python-shell-send-defun
   "C-<return>" 'python-shell-send-line)

  (defvar pdb-completion-at-point-script
    (concat
     "import rlcompleter;"
     "__ECAP_N=locals().copy();__ECAP_N.update(**globals());"
     "__ECAP_T='''%s''';"
     "print(';'.join("
     "getattr(rlcompleter.Completer(__ECAP_N), 'attr_matches' if '.' in __ECAP_T else 'global_matches')"
     "(__ECAP_T)))"))

  (defun pdb-completion-at-point-function ()
    "Complete at point if in pdb prompt."
    (let* ((buffer (current-buffer))
           (process (get-buffer-process buffer)))
      (defvar python-shell-prompt-pdb-regexp)
      ;; Get completion only if there are process and we are at pdb prompt
      (when (and
             process
             (save-excursion
               (forward-line 0)
               (looking-at python-shell-prompt-pdb-regexp)))
        (let* ((end (point))
               ;; Beginning of prefix to search completion for
               ;; Get whole input and search backwards for delimiters
               (start (save-excursion (comint-goto-process-mark) (point)))
               (start (or (save-excursion
                            (when (re-search-backward
                                   "[([{]\\|[])}]\\|[[:space:]]\\|\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
                                   start t)
                              (1+ (point))))
                          start)))
          (when (> end start)
            (let ((prefix (buffer-substring-no-properties start end))
                  ;; this regexp is used to determine that output redirect is done
                  (comint-prompt-regexp (concat "^" python-shell-prompt-pdb-regexp)))
              (with-temp-buffer
                (comint-redirect-send-command-to-process
                 (format pdb-completion-at-point-script prefix)
                 (current-buffer) process nil t)
                ;; wait for command output
                (with-current-buffer buffer
                  (while (null comint-redirect-completed)
                    (accept-process-output nil 0.1)))
                (goto-char (point-min))
                ;; Skip first line in case process echoes input
                (unless (= (count-lines (point-min) (point-max)) 1)
                  (forward-line 1))
                (let ((completions (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                  ;; return nil instead of empty completions so other
                  ;; completion functions can provide their completions
                  (unless (string-empty-p completions)
                    (list start end (split-string completions ";")))))))))))

  (add-hook
   'inferior-python-mode-hook
   (lambda ()
     (add-hook 'completion-at-point-functions
               'pdb-completion-at-point-function nil t))))

(defun pdb-poetry ()
  "Like `py-pdb' but with poetry."
  (interactive)
  (require 'gud)
  (let ((command-line
         (read-from-minibuffer
          "Run pdb like this: "
          (concat
           "poetry run python3 -m pdb "
           (if (and (featurep 'tramp) (tramp-tramp-file-p (buffer-file-name)))
               (tramp-file-name-localname (tramp-dssect-file-name (buffer-file-name)))
             (buffer-file-name))))))
    (if command-line (pdb command-line) (error "command required"))))

(package! elpy (:host github :repo "jorgenschaefer/elpy")
  :hook ((python-mode ein-mode) . elpy-mode)
  :custom
  (elpy-rpc-virtualenv-path 'default)
  (elpy-rpc-python-command "python3")
  (py-return-key #'py-newline-and-indent)

  :config
  (elpy-enable)

  (defun elpy-goto-import-header ()
    "Jump to the import header."
    (interactive)
    (evil-set-jump)
    (goto-char (point-min))
    (re-search-forward "^import"))

  (defun elpy-shell-goto-last-error ()
    "Jump to the last error reported by the shell."
    (interactive)
    (let ((filename (buffer-file-name)))
      (elpy-shell-switch-to-shell)
      (goto-char (point-max))
      (search-backward filename nil t)
      (while (condition-case nil
                 (progn (compile-goto-error) nil)
               (error t))
        (forward-line -1))))

  (aero-mode-leader-def
    :keymaps 'elpy-mode-map
    "p" 'run-python
    "'" 'elpy-shell-switch-to-shell
    "s" '(:ignore t :wk "shell send")
    "ss" '(elpy-shell-send-region-or-buffer :wk "send region or buffer")
    "sS" '(elpy-shell-send-region-or-buffer-and-go :wk "send region or buffer and go")
    "sd" '(elpy-shell-send-defun :wk "send defun")
    "sD" '(elpy-shell-send-defun-and-go :wk "send defun and go")
    "sb" '(elpy-shell-send-buffer :wk "send buffer")
    "sB" '(elpy-shell-send-buffer-and-go :wk "send buffer and go")
    "sc" '(elpy-shell-send-defclass :wk "send defclass")
    "sC" '(elpy-shell-send-defclass-and-go :wk "send defclass and go")
    "k" '(:ignore t :wk "kill")
    "kk" '(elpy-shell-kill :wk "kill shell")
    "kA" '(elpy-shell-kill-all :wk "kill all shells")
    "g" '(:ignore t :wk "go")
    "ge" '(elpy-shell-goto-last-error :wk "goto last error")
    "gi" '(elpy-shell-goto-import-header :wk "goto import header")
    "t" '(elpy-test :wk "run test")
    "d" '(:ignore t :wk "debug")
    "dd" '(elpy-pdb-debug-buffer :wk "debug buffer")
    "db" '(elpy-pdb-toggle-breakpoint-at-point :wk "toggle breakpoint")
    "r" '(:ignore t :wk "refactor")
    "rb" '(elpy-black-fix-code :wk "black format")
    "rr" '(elpy-refactor-rename :wk "rename")
    "rv" '(elpy-refactor-extract-variable :wk "extract variable")
    "rf" '(elpy-refactor-extract-function :wk "extract function")
    "ri" '(elpy-refactor-inline :wk "inline variable")
    "rF" '(elpy-format-code :wk "format buffer or region")))

(package! flymake-mypy "com4/flymake-mypy"
  :after (eglot)
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-base-mode)
                (flymake-mypy-enable)))))

(package! flymake-ruff "erickgnavar/flymake-ruff"
  :after (eglot)
  :functions (flymake-ruff-load)
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (derived-mode-p 'python-base-mode)
                  (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
                  (flymake-ruff-load))))))


;; Rust

(package! rust-mode :auto
  :mode "\\.rs\\'"
  :config
  (setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))
  (require 'company)
  (defvar company-tooltip-align-annotations)
  (declare-function company-indent-or-complete-common "company")
  (setq company-tooltip-align-annotations t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode t))))

(package! cargo :auto
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(package! toml-mode :auto
  :mode "\\(\\.toml\\|Cargo\\.lock\\)\\'")


;; parentheses

(package! smartparens "Fuco1/smartparens"
  :after (general) :defer 5
  :functions (show-smartparens-global-mode
              sp-kill-sexp sp-local-pair
              sp-local-pairs sp-pair
              sp-up-sexp)
  :after (evil general)
  :hook ((after-init . smartparens-global-mode))
  :init
  ;; fix highlighting in normal mode
  (setq sp-show-pair-from-inside t)
  (smartparens-global-mode +1)
  :config
  ;; from spacemacs
  (defun aero/smart-closing-parenthesis ()
    "Insert a closing pair delimiter or move point past existing delimiter.

If the expression at point is already balanced and there is a closing delimiter
for that expression on the current line, move point forward past the closing
delimiter. If the expression is balanced but there is no closing delimiter on
the current line, insert a literal ')' character. If the expression is not
balanced, insert a closing delimiter for the current expression. This command
uses Smartparens navigation commands and therefore recognizes pair delimiters
that have been defined using `sp-pair' or `sp-local-pair'."
    (interactive)
    (let* ((sp-navigate-close-if-unbalanced t)
           (current-pos (point))
           (current-line (line-number-at-pos current-pos))
           next-pos next-line)
      (aero/voidvar! sp-navigate-close-if-unbalanced)
      (save-excursion
        (let ((buffer-undo-list)
              (modified (buffer-modified-p)))
          (unwind-protect
              (progn
                (sp-up-sexp)
                (setq next-pos (point)
                      next-line (line-number-at-pos)))
            (primitive-undo (length buffer-undo-list)
                            buffer-undo-list)
            (set-buffer-modified-p modified))))
      (cond
       ((and (= current-line next-line)
             (not (= current-pos next-pos)))
        (sp-up-sexp))
       (t
        (insert-char ?\))))))

  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  (defun aero/copy-sexp-as-kill (&optional arg)
    "Copy the sexp to the kill ring without killing."
    (interactive)
    (funcall #'sp-kill-sexp arg t))

  (defun aero/sp-wrap-double-quote () (interactive) (sp-wrap-with-pair "\""))
  (defun aero/sp-wrap-single-quote () (interactive) (sp-wrap-with-pair "'"))
  (defun aero/sp-wrap-backtick () (interactive) (sp-wrap-with-pair "`"))
  (defun aero/sp-wrap-angle () (interactive) (sp-wrap-with-pair "<"))

  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "s^" '(sp-beginning-of-sexp :which-key "beginning of sexp")
   "s$" '(sp-end-of-sexp :which-key "end of sexp")
   "sh" '(sp-backward-sexp :which-key "back")
   "sl" '(sp-forward-sexp :which-key "forward")
   "sw" '(:ignore t :which-key "wrap")
   "sw(" 'sp-wrap-round
   "sw{" 'sp-wrap-curly
   "sw[" 'sp-wrap-square
   "sw<" 'aero/sp-wrap-angle
   "sw\"" '(aero/sp-wrap-double-quote :wk "wrap double quote")
   "sw'" '(aero/sp-wrap-single-quote :wk "wrap single quote")
   "sw`" '(aero/sp-wrap-backtick :wk "wrap backtick")
   "swr" 'sp-rewrap-sexp
   "su" '(sp-unwrap-sexp :which-key "unwrap")
   "sk" '(sp-kill-sexp :which-key "kill")
   "sK" '(aero/copy-sexp-as-kill :wk "copy as kill"))

  (sp-local-pair 'web-mode "<?" "?>")
  (sp-local-pair 'web-mode "{" "}")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "{%" "%}")
  (sp-local-pair 'web-mode "{% " " %}")
  (sp-local-pair 'web-mode "{{ " " }}")
  (sp-local-pair 'web-mode "`" "`")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-local-pair 'markdown-mode "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

  (sp-pair "<" ">")

  ;; For these pairs, when hitting RET inside them, we add an extra newline to the middle and indent
  ;; accordingly.
  (sp-pair "{" "}" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "[" "]" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "(" ")" :post-handlers '(:add ("||\n[i]" "RET")))

  (define-key evil-insert-state-map ")" 'aero/smart-closing-parenthesis))

(package! rainbow-delimiters "Fanael/rainbow-delimiters"
  :hook ((prog-mode . rainbow-delimiters-mode)))


;;; formatting & debugging

(package! apheleia
  (:repo "radian-software/apheleia"
   :files (:defaults ("scripts" "scripts/formatters")))
  :after general
  :init (apheleia-global-mode +1)
  :config
  (dolist (cmd `((elm-format . (npx "elm-format" "--yes" "--stdin"))
                 (cljfmt . ("lein" "cljfmt" "fix" filepath))))
    (add-to-list 'apheleia-formatters cmd))

  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljfmt))

  (aero-leader-def
    "bI" 'apheleia-format-buffer))

;; Debug adapter, see https://github.com/svaante/dape for setup instructions, there's a lot of
;; manual setup required
(package! dape "svaante/dape"
  :after (general project eglot)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-adapter-dir (expand-file-name "debug-adapters/" user-emacs-directory))
  (dape-inlay-hints t)
  :config
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  (setq dape-configs-port 4711)

  (defun aero/install-vscode-js-debug ()
    "Run installation procedure to install JS debugging support"
    (interactive)
    (let ((vscode-js-debug-dir (expand-file-name "js-debug" dape-adapter-dir)))
      (mkdir vscode-js-debug-dir t)
      (let ((default-directory (expand-file-name vscode-js-debug-dir)))
        (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
        (message "git repository created")
        (call-process "npm" nil "*aero-install*" t "install")
        (message "npm dependencies installed")
        (call-process "npx" nil "*aero-install*" t "gulp" "dapDebugServer")
        (message "vscode-js-debug installed"))))

  ;; Set up JS
  (add-to-list 'dape-configs
               `(js-debug-node
                 modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 ;; Command to start the debug adapter
                 command "node"
                 command-cwd ,(concat dape-adapter-dir "js-debug")
                 command-args ("src/dapDebugServer.js" ,(format "%d" dape-configs-port))
                 ;; Port that Emacs will connect to the debug adapter
                 port ,dape-configs-port
                 ;; Debug configuration
                 :type "pwa-node"
                 :request "attach"
                 :name "Attach to Jest Test"
                 :address "localhost"
                 :port 9229 ;; Node.js debug port (from --inspect-brk)
                 :localRoot dape-cwd-fn
                 :remoteRoot nil
                 :skipFiles ["<node_internals>/**" "**/node_modules/**"]
                 :resolveSourceMapLocations ["!**/node_modules/**" "**/*"]
                 :suppressSourceMapWarning t
                 )))



;;; auto modes and stuff

(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))
;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
(add-to-list 'auto-mode-alist '("/\\.dir-locals\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/Cask\\'" . emacs-lisp-mode))

;; somehow makefile-mode stopped activating?
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))


;;; whitespace and indentation and stuff

(package! ws-butler :auto
  :functions (ws-butler-global-mode)
  :init (ws-butler-global-mode)
  :custom
  ;; default is just markdown-mode, which is a mode where I really want this in particular. Instead,
  ;; only exempt modes where whitespace could be important.
  (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode)))

;; Ocaml
(package! tuareg :auto :mode ("\\.mli?\\'" . tuareg-mode))

;; SQL
(package! sql :builtin
  :defer t
  :after (general)
  :commands (sql-connect)
  :init
  (aero-leader-def
    "Sc" 'sql-connect)

  :config
  (setq sql-sqlite-program "sqlite3")

  (aero-mode-leader-def
    :keymaps 'sql-mode-map
    "b" 'sql-send-buffer
    "B" 'aero/sql-send-buffer-and-focus
    "r" 'sql-send-region
    "R" 'aero/sql-send-region-and-focus
    "p" 'sql-send-paragraph
    "P" 'aero/sql-send-paragraph-and-focus
    "s" 'sql-send-string
    "S" 'aero/sql-send-string-and-focus)

  ;; for sql comint
  (add-to-list 'same-window-buffer-names "*SQL: *")
  (add-hook 'sql-interactive-mode-hook #'evil-insert-state)

  (defun aero/sql-send-string-and-focus ()
    "Send a string to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (call-interactively 'sql-send-string)
      (evil-insert-state)))

  (defun aero/sql-send-buffer-and-focus ()
    "Send the buffer to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-buffer)
      (evil-insert-state)))

  (defun aero/sql-send-paragraph-and-focus ()
    "Send the paragraph to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-paragraph)
      (evil-insert-state)))

  (defun aero/sql-send-region-and-focus (start end)
    "Send region to SQLi and switch to SQLi in `insert state'."
    (interactive "r")
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-region start end)
      (evil-insert-state)))

  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook))

;; Docker

(package! docker-compose-mode :auto :mode "docker-compose.*\.yml\\'")
(package! dockerfile-mode :auto :mode "Dockerfile[a-zA-Z.-]*\\'")


;;; additional packages which might not fit elsewhere

(package! nix-mode :auto :mode "\\.nix\\'")
(package! lua-mode :auto :mode "\\.lua\\'")
(package! applescript-mode :auto :mode "\\.applescript\\'")
(package! nhexl-mode :auto :defer t) ; improved version of `hexl-mode'
(package! pdf-tools :auto :defer t)
(package! terraform-mode :auto :mode "\\.tf\\'")
(package! glsl-mode (:repo "jimhourihan/glsl-mode")
  :mode "\\.\\(vert\\|frag\\)\\'"
  :config (add-hook 'glsl-mode-hook #'(lambda () (setq tab-width 4 c-basic-offset 4))))
(package! graphql-mode :auto :mode "\\.graphql\\'")
(package! groovy-mode :auto :mode "\\(\\.groovy\\'\\|Jenkinsfile\\)")
(package! csv-mode :auto :mode "\\.csv\\'")
(package! logstash-conf :auto :commands (logstash-conf-mode))

(add-hook
 'orson-mode-hook
 (lambda ()
   (setq-local indent-tabs-mode nil)
   (prettify-symbols-mode nil)))

;; Elm-mode is supposed to enable elm-indent-mode by default, but for some reason it stopped doing
;; this on Dec 21, 2022. Probably caused this myself, but this fixes it.
(package! elm-mode :auto :hook (elm-mode . elm-indent-mode))

(package! tcl :builtin
  :mode ("\\(\\.tcl\\|\\.test\\)\\'" . tcl-mode)
  :custom
  (tcl-application "tclsh")
  :config
  (add-to-list 'tcl-type-alist '("namespace" "eval" tcl-expr tcl-commands)))

(package! rivet-mode "thornjad/rivet-mode")


;;; Git

(package! magit :auto
  :after (general)
  :commands (magit-blame
             magit-commit
             magit-diff-unstaged
             magit-init
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :init
  (aero-leader-def
    "gs" 'magit-status
    "gb" 'magit-blame
    "gl" '(:ignore t :which-key "log")
    "glb" 'magit-log-buffer-file
    "gld" 'magit-log-trace-definition
    "gll" 'magit-log-head
    "gfS" 'magit-stage-file
    "gfU" 'magit-unstage-file
    "gm" '(:ignore t :which-key "smerge")
    "gmm" 'smerge-start-session
    "gmu" 'smerge-keep-upper
    "gml" 'smerge-keep-lower
    "gmn" 'smerge-next
    "gmp" 'smerge-prev
    "gma" 'smerge-keep-all
    "gmE" 'smerge-ediff)

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-process-finish-apply-ansi-colors t)
  (magit-buffer-name-format "%x%M%v: %t%x")
  (magit-list-refs-sortby "-creatordate")
  (magit-diff-paint-whitespace-lines 'both)
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace t)
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)

  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (magit-auto-revert-mode nil)

  (defun aero/truncate-lines-off () (toggle-truncate-lines -1))
  (add-hook 'magit-status-mode-hook #'aero/truncate-lines-off)

  (defun aero/magit-switch-to-diff () (other-window 1))
  (advice-add 'magit-diff :after #'aero/magit-switch-to-diff)

  (defun aero/magit-diff-default-branch (&optional args)
    "Show diff of default branch to working tree."
    (interactive (list (magit-diff-arguments)))
    (magit-diff-working-tree
     (replace-regexp-in-string "refs/remotes/origin/" ""
                               (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))
     args))

  ;; Don't want no color from the pre-commit hook
  (defun aero/magit--color-buffer (proc &rest args)
    (interactive)
    (with-current-buffer (process-buffer proc)
      (read-only-mode -1)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1)))
  (advice-add 'magit-process-filter :after #'aero/magit--color-buffer)

  (defun aero/fetch-pr ()
    "Fetch a GH(E) pull request into a new branch prefixed with `pr'."
    (interactive)
    (let* ((pr (message-read-from-minibuffer "Enter PR number: "))
           (new-branch (format "pr%s" pr))
           (fetch-command
            (format "git fetch origin pull/%s/head:%s" pr new-branch)))
      (shell-command fetch-command)
      (magit-status)
      (message "Checked out PR as %s" new-branch))))

(package! git-gutter :auto
  :hook ((prog-mode text-mode conf-mode) . git-gutter-mode)
  :custom
  (git-gutter:visual-line t)
  (git-gutter:disabled-modes '(so-long-mode
                               image-mode asm-mode
                               doc-view-mode
                               fundamental-mode image-mode pdf-view-mode))
  (git-gutter:update-interval 0.02)
  (git-gutter:handled-backends
   (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                :key #'symbol-name)))

  :config
  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows))

(when (display-graphic-p)
  ;; Define as a no-op if not already defined, otherwise git-gutter-fringe errors
  (unless (fboundp 'define-fringe-bitmap)
    (defun define-fringe-bitmap (bitmap &rest _)
      "This is a no-op placeholder function."
      ;; Return the symbol, just like the normal function does.
      bitmap))

  (package! git-gutter-fringe :auto :after (git-gutter)
    :custom
    (fringes-outside-margins t)

    :config
    ;; Define a thin bar. Themes should give these a suitable foreground and nil background
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'bottom)

    ;; Don't use git-gutter in TRAMP, it murders connection bandwidth
    (defun git-gutter-find-file-hook ()
      (git-gutter-mode
       (if (file-remote-p (buffer-file-name))
           0
         1)))
    (add-hook 'find-file-hook #'git-gutter-find-file-hook)))

(package! ediff :builtin
  :commands (ediff ediff3)
  :custom
  (ediff-split-window-function #'split-window-horizontally )
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(package! git-link :auto
  :after (general)
  :commands (git-link git-link-commit git-link-homepage)
  :init (aero-leader-def "gL" 'git-link))


;;; Org, Thornlog, Agenda

(package! org :builtin
  :preface
  (defun archive-buffer-closed-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (when (member (org-get-todo-state) org-done-keywords)
         (org-archive-subtree-default)
         ;; the archive alters the tree, so just go back to the top
         (setq org-map-continue-from (point-min))))
     nil 'file))

  (defun trim-archive-entries ()
    "Trim entries in the archive file older than 60 days."
    (interactive)
    (let ((archive-file (expand-file-name aero/thornlog-archive-file)))
      (when (file-exists-p archive-file)
        (let ((cutoff-date (time-subtract (current-time) (days-to-time 60))))
          (with-current-buffer (find-file-noselect archive-file)
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (org-at-heading-p)
                         (let ((archive-time-str (org-entry-get (point) "ARCHIVE_TIME")))
                           (and archive-time-str
                                (time-less-p (org-read-date nil t archive-time-str) cutoff-date))))
                (org-cut-subtree)
                (org-back-to-heading t)
                (outline-previous-heading))
              (outline-next-heading)))
          (save-buffer)))))

  (defun aero/org-archive-cleanup ()
    "Archive closed tasks and trim archive entries."
    (interactive)
    (archive-buffer-closed-tasks)
    (trim-archive-entries))

  (defun aero/org-collapse-entry-if-done ()
    "Collapse the current entry if it is marked as DONE."
    (when (member org-state '("DONE"))
      (hide-subtree)))

  (defun aero/org-expand-entry-if-todo ()
    "Expand the current entry if it is marked as TODO."
    (when (member org-state '("TODO"))
      (show-subtree)))

  (defun jump-to-org-agenda ()
    "Go to the org agenda. Used on idle timer."
    (interactive)
    (let ((buf (get-buffer "*Org Agenda*"))
          wind)
      (if buf
          (if (setq wind (get-buffer-window buf))
              (select-window wind)
            (if (called-interactively-p 'any)
                (progn
                  (select-window (display-buffer buf t t))
                  (org-fit-window-to-buffer))
              (with-selected-window (display-buffer buf)
                (org-fit-window-to-buffer))))
        (call-interactively 'org-agenda-list))))

  ;; https://ag91.github.io/blog/2022/03/12/org-agenda-keep-the-buffer-order-untouched-after-saving-all-modified-org-buffers/
  (defun aero/reorder-buffer-list (new-list)
    (while new-list
      (bury-buffer (car new-list))
      (setq new-list (cdr new-list))))

  (defun aero/keep-buffer-list-unaltered (orig-fun &rest args)
    (let ((buffers (buffer-list))
          (result (apply orig-fun args)))
      (aero/reorder-buffer-list buffers)
      result))

  (defun org-schedule-and-refile ()
    "Schedule the current entry and refile it."
    (interactive)
    (org-schedule t)
    (org-refile))

  (defun org-deadline-and-refile ()
    "Deadline the current entry and refile it."
    (interactive)
    (org-deadline t)
    (org-refile))

  (defun aero/org-agenda-format-date (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date))
	         (day (cadr date))
	         (month (car date))
	         (monthname (calendar-month-name month))

           ;; divisor must be float so (/) doesn't do integer division
           (quarter (round (/ (1+ month) 3.0)))

	         (year (nth 2 date))
	         (iso-week (org-days-to-iso-week
		                  (calendar-absolute-from-gregorian date)))
	         (day-of-week (calendar-day-of-week date))
	         (weekstring (if (= day-of-week 1)
			                     (format " W%02d" iso-week)
		                     "")))
      (format "%-10s %2d %s %4d%s   (Q%s)"
	            dayname day monthname year weekstring quarter)))

  (defun aero/org-deindent-on-return (&rest _)
    "De-indent the current line if there is only whitespace before the point when pressing ENTER.

This behavior is IDIOTIC and I cannot suffer to live with this automatic indentation any longer."
    (when (and (derived-mode-p 'org-mode)
               (save-excursion
                 (move-beginning-of-line 1)
                 (looking-at-p "[ \t]*$")))
      (delete-horizontal-space)))

  :custom
  (org-hide-leading-stars t)
  (org-pretty-entities t)
  (org-indent-mode-turns-on-hiding-stars nil) ; why would this even exist??
  (org-fontify-quote-and-verse-blocks t)
  (org-insert-heading-respect-content t) ; insert headings after current subtree
  (org-fold-catch-invisible-edits 'smart) ; don't accidentally remove hidden text
  (org-startup-with-inline-images t) ; default to showing images on startup
  (org-startup-indented t)
  (org-log-done 'time) ; log time when item is marked done
  (org-log-into-drawer t) ; put logs in LOGBOOK
  (org-refile-use-outline-path t) ; show path to outline level during refile
  (org-fontify-done-headline t) ; let theme strike out done items
  (org-return-follows-link t) ; follow links with RET

  ;; always put blank before new headings, but be smart about list items
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  ;; re-scale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (org-image-actual-width '(400))

  (org-capture-templates
   `(("t" "Deadline/Scheduled Task" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/roam-path)
       "Tasks")
      "* TODO [#C] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
      :empty-lines 1)
     ("p" "Ticket (PR)" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/roam-path)
       "Tasks")
      "* TICKET [#C] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("r" "Review (PR or tech design)" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/roam-path)
       "Tasks")
      "* REVIEW [#B] %? :review:\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\nLink: "
      :empty-lines 1)
     ("s" "School inbox item" entry
      (file+headline
       ,(expand-file-name "school_todo.org" aero/roam-path)
       "Tasks")
      "* TODO [#C] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
      :empty-lines 1)
     ("n" "Note" entry
      (file+headline
       ,(expand-file-name "notes_inbox.org" aero/roam-path)
       "Notes")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :jump-to-captured t
      :empty-lines 1)
     ("e" "Experimentation idea" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/roam-path)
       "Experimentation")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
      :empty-lines 1)
     ("R" "Reading" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/roam-path)
       "Reading")
      "* TODO [#E] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("s" "Time sink" entry
      (file+headline
       ,(expand-file-name "20250123102747-time_sinks_at_dd.org" aero/roam-path)
       "Time sinks")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)
     ("m" "Mistake" entry
      (file+headline
       ,(expand-file-name "20250123103552-mistakes_to_learn_from_at_dd.org" aero/roam-path)
       "Mistakes")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 1)))

  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w!)" "BLOCKED(b!)" "BACKLOG(l!)" "|" "DONE(d!)" "REMOVED(k)")
     (sequence "TICKET(T)" "PR(p!)" "|" "DONE(d!)" "CLOSED(x)")
     (sequence "REVIEW(r)" "WAITING(w!)" "BLOCKED(b!)" "|" "DONE(d!)" "CLOSED(x)")))

  (org-use-fast-todo-selection 'expert) ; don't fuck up the window layout
  (org-default-notes-file (expand-file-name "notes_inbox.org" aero/roam-path))
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)
                        (?D . org-priority)
                        (?E . org-priority)))
  (org-priority-highest ?A)
  (org-priority-lowest ?E) ; default is C
  (org-reverse-note-order nil) ; put notes at the end of the entry, instead of the top
  (org-archive-location (concat aero/thornlog-archive-file "::* From %s"))

  ;; don't consider empty lines between entries to be part of the entry
  (org-cycle-separator-lines -2)

  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))

  ;; Agenda
  (org-agenda-span 1) ; days to show at a time
  (org-agenda-start-day nil) ; day to start at
  (org-agenda-start-on-weekday nil) ; start week on current day
  (org-agenda-format-date #'aero/org-agenda-format-date)
  (org-agenda-prefix-format '((agenda . "  %?-12t%s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))

  ;; all agenda files
  (org-agenda-files `(,(expand-file-name "todo.org" aero/roam-path)
                      ,(expand-file-name "log.org" aero/roam-path)
                      ,(expand-file-name "ritual.org" aero/roam-path)
                      ,(expand-file-name "holidays.org" aero/roam-path)
                      ,(expand-file-name "notes_inbox.org" aero/roam-path)))

  ;; holidays I don't want to display
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-christian-holidays nil)
  (holiday-oriental-holidays nil)

  ;; local holidays
  (holiday-local-holidays '((holiday-fixed 3 14 "Pi Day")
                            (holiday-fixed 10 23 "Mole Day")
                            (holiday-fixed 11 23 "Fibonacci Day")
                            (holiday-fixed 12 23 "Festivus")
                            (holiday-fixed 9 19 "Talk Like a Pirate Day")
                            (holiday-fixed 10 9 "Leif Erikson Day")
                            (holiday-fixed 5 4 "Star Wars Day")
                            (holiday-fixed 6 28 "Tau Day")

                            (holiday-fixed 2 27 "Hangover (first day)")
                            (holiday-fixed 2 28 "Hangover (second day")
                            (holiday-fixed 2 29 "Hangover (third day")
                            (holiday-fixed 3 1 "Hangover (fourth day")
                            (holiday-fixed 3 2 "Hangover (fifth day")
                            (holiday-fixed 3 3 "Hangover (sixth day")
                            (holiday-fixed 3 4 "Hangover (seventh day")
                            (holiday-fixed 3 5 "Hangover (eighth day")
                            (holiday-fixed 3 6 "The Day of the Dude")))

  (org-agenda-log-mode-items nil) ; don't show closed nor clocked items
  (org-agenda-tags-column -70) ; shift tags over
  (org-agenda-sticky nil) ; don't bury on close buffer
  (org-agenda-use-tag-inheritance t)
  (org-agenda-show-log t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t) ; don't duplicate deadline & scheduled
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-window-setup 'current-window) ; stop agenda opening a new window
  (org-agenda-skip-unavailable-files t)
  (org-agenda-show-future-repeats nil) ; don't show repeating tasks on future agenda dates
  (org-agenda-custom-commands
   `(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
     ("s" "School items" agenda ""
      ((org-agenda-files '(,(expand-file-name "school_todo.org" aero/roam-path)))))
     ("e" "Experimentation tag" tags "experimentation")))

  :init
  (aero-leader-def
    "oa" '(aero/org-agenda-list :wk "agenda")
    "oc" '(aero/org-class-agenda-list :wk "class agenda")
    "oA" '(org-agenda :wk "agenda menu")
    "os" 'org-schedule
    "od" 'org-deadline
    "oj" 'org-clock-goto
    "ot" 'aero/org-set-tags
    "ol" 'org-store-link
    "oT" '(org-tags-view :wk "list tags")
    "vo" 'org-capture)

  :config
  (aero-mode-leader-def
    :keymaps 'org-mode-map
    "t" 'org-todo
    "f" 'org-forward-heading-same-level
    "F" 'org-backward-heading-same-level
    "w" 'org-open-at-point
    "p" 'org-priority
    "r" 'org-refile
    "s" 'org-set-property
    "i" '(:ignore t :wk "insert")
    "il" '(org-insert-link :wk "link")
    "it" 'org-time-stamp
    "ii" 'org-insert-structure-template
    "id" '(org-insert-drawer :wk "drawer")
    "im" 'insert-meeting-task
    "is" 'insert-class-task
    "A" 'aero/org-archive-cleanup
    "c" '(:ignore t :wk "clock / cell")
    "cc" '(org-babel-execute-src-block :wk "exec cell")
    "ci" 'org-clock-in
    "co" 'org-clock-out
    "ck" 'org-clock-cancel
    "cs" 'org-clock-display
    "ce" 'org-set-effort
    "cE" 'org-clock-modify-effort-estimate)

  ;; Make scheduled previously look the same as scheduled today, without messing with the theme
  (set-face-attribute 'org-scheduled-previously nil :inherit 'org-scheduled-today :foreground nil)

  ;; keep org-save-all from messing up buffer list
  (advice-add 'org-save-all-org-buffers :around #'aero/keep-buffer-list-unaltered)

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; Collapse entries when they are marked as done, and expand when reopened
  (add-hook 'org-after-todo-state-change-hook #'aero/org-collapse-entry-if-done)
  (add-hook 'org-after-todo-state-change-hook #'aero/org-expand-entry-if-todo)

  ;; Get rid of the idiotic indentation after pressing enter
  (advice-add 'org-return :after #'aero/org-deindent-on-return)
  (with-eval-after-load 'evil
    (advice-add 'evil-org-open-below :after #'aero/org-deindent-on-return))

  ;; Also save after state change
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all)

  ;; Save org files when using clock
  (add-hook 'org-clock-in-hook #'org-save-all-org-buffers)
  (add-hook 'org-clock-out-hook #'org-save-all-org-buffers)

  ;; Force org-capture to not open new windows
  (defun aero/org-capture-place-template-dont-delete-windows (oldfun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))
  (with-eval-after-load "org-capture"
    (advice-add 'org-capture-place-template
                :around #'aero/org-capture-place-template-dont-delete-windows))

  ;; set up stuff for clock persistence
  (org-clock-persistence-insinuate)

  ;; Show agenda when Emacs is idle for 10 minutes, from
  ;; https://sachachua.com/dotemacs/index.html#idle-timer
  (run-with-idle-timer 600 t 'jump-to-org-agenda))

;; Show formatting markers when point is near
(package! org-appear "awth13/org-appear"
  :custom (org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

;; Org-mode UI improvements
(package! org-modern "minad/org-modern"
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom
  (org-modern-star 'replace) ; uses the "replace" set of stars
  )

;; Use evil in org, particularly in org-agenda. Also unblocks using aero-leader chords. See
;; https://github.com/Somelauw/evil-org-mode for a list of available commands
(package! evil-org-mode "Somelauw/evil-org-mode"
  :after (evil org org-super-agenda)
  :preface
  (defun aero/evil-org-agenda-mode ()
    "Shim in org-agenda evil mode."
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . aero/evil-org-agenda-mode)))

;; Custom display of org priorities
(package! org-fancy-priorities "harrybournis/org-fancy-priorities"
  :after (org)
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("!!" "↑" "·" "↓" "_")))

(package! org-super-agenda "alphapapa/org-super-agenda"
  :preface
  (defun aero/org-super-agenda-without-keymap ()
    "Stops super-agenda from overriding evil-org bindings."
    (org-super-agenda-mode)
    (setq org-super-agenda-header-map (make-sparse-keymap)))

  :hook ((org-agenda-after-show . recenter)
         (org-agenda-mode . aero/org-super-agenda-without-keymap))

  :custom
  (org-super-agenda-groups
   '((:name "Daily Routine" :and (:tag "ritual" :not (:time-grid t)))
     (:name "Holidays" :tag "holiday" :category ("Holiday" "Anniversaries"))
     (:name "Outstanding meetings" :and (:scheduled past :tag "meeting"))
     (:time-grid t)
     (:name "Reviews to do" :and (:tag "review" :todo "REVIEW" :not (:todo ("WAITING" "BLOCKED" "BACKLOG"))))
     (:name "Support" :and (:tag "support"))
     (:name "Waiting/blocked" :todo ("WAITING" "BLOCKED"))
     (:name "Past due" :and (:deadline past :not (:todo ("WAITING" "BLOCKED"))))
     (:name "Due today" :and (:deadline today :not (:todo ("WAITING" "BLOCKED"))))
     (:name "Prioritized" :not (:todo ("WAITING" "BLOCKED" "BACKLOG")))
     (:name "Backlog" :and (:todo "BACKLOG"))))

  ;; add space between dates by adding space after the final group
  (org-super-agenda-final-group-separator "\n"))

;; Allow drag-and-drop of images from browser, finder, etc.
(package! org-download "abo-abo/org-download"
  :after (org general)
  :custom (org-download-method 'directory)
  :init
  (aero-mode-leader-def
    :keymaps 'org-mode-map
    "ic" '(org-download-clipboard :wk "insert image from clipboard")))


;; Functions for agenda and stuff

(defun aero/org-agenda-list ()
  "`org-agenda', skipping command menu to list."
  (interactive)
  (org-agenda nil "a"))

(defun aero/org-class-agenda-list ()
  "`org-agenda', skipping command menu to list."
  (interactive)
  (org-agenda nil "s"))

(defun aero/org-agenda-todo ()
  "`org-agenda', skipping command menu to todos."
  (interactive)
  (org-agenda nil "t"))

(defun aero/org-agenda-new ()
  "Create a new task at the current agenda item."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(defun org-agenda-list-closed-on-last-workday ()
  (interactive)
  (let* ((org-agenda-files (list (buffer-file-name)
                                 (expand-file-name "archive/archive.org" aero/thornlog-path)
                                 (expand-file-name "log.org" aero/roam-path)))
         (today (current-time))
         (weekday (format-time-string "%u" today))
         (days-back (if (string= weekday "1") 3 1)) ; If today is Monday (1), go back 3 days to Friday
         (specific-day (format-time-string "%Y-%m-%d" (time-subtract today (days-to-time days-back))))
         (org-agenda-log-mode-items '(closed))
         (org-agenda-skip-deadline-if-done nil)
         (org-agenda-skip-scheduled-if-done nil)
         (org-agenda-skipp-timestamp-if-done nil)
         (org-super-agenda-groups '((:time-grid t))))
    (org-agenda-list nil specific-day 'day)))

(defun insert-todays-timestamp-at-entry-end ()
  "Insert today's timestamp at the end of the current org entry."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (end-of-line)
    (insert " ")
    (org-insert-time-stamp (current-time) nil)))


;; Thornlog management

(defun aero/thornlog-log ()
  "Personal persistent log."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-title-or-alias "Work Log")))

(defun aero/thornlog-todo ()
  "Personal todo list."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-title-or-alias "Work Todo: Triaged Tasks and Inbox")))

(defun aero/thornlog-clean-save ()
  "Automates the git commit in thornlog."
  (interactive)
  (let* ((default-directory aero/thornlog-path)
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))
    (save-some-buffers t)
    (let ((todo-file (expand-file-name "todo.org" aero/roam-path)))
      (when (file-exists-p todo-file)
        (with-current-buffer (find-file-noselect todo-file)
          (aero/org-archive-cleanup)
          (save-some-buffers t))))
    (shell-command "git fetch origin")
    (if (not (zerop (shell-command "git rev-list --count @{u}..")))
        (user-error "Remote has changes, manual commit required")
      (progn
        (shell-command "git add -A")
        (shell-command (format "git commit -m '%s'" timestamp))
        (if (not (zerop (shell-command "git push origin")))
            (user-error "Git push failed, manual inspection required")
          (message "Done with Thornlog commit and push"))))))

(defun insert-meeting-task ()
  (interactive)
  (let* ((meeting-name (read-string "Meeting Name: "))
         (meeting-time (read-string "Meeting Time (optional): "))
         (today (format-time-string "%Y-%m-%d"))
         (scheduled-string (if (not (string= meeting-time ""))
                               (format "<%s %s>" today meeting-time)
                             (format "<%s>" today)))
         (task-string (format "*** MEETING %s  :meeting:\nSCHEDULED: %s"
                              meeting-name scheduled-string)))
    (goto-char (point-max))
    (re-search-backward "^\\*+ Meetings" nil t)
    (org-end-of-subtree)
    (insert "\n\n" task-string)))

(defun aero/org-add-file-tag ()
  "Prompts for a tag with completion from all org-roam tags and adds it to the file's tags, placing it after the #+title: line if it exists."
  (interactive)
  (let* ((case-fold-search t)
         (all-tags-query "SELECT DISTINCT tag FROM tags")
         (all-tags-result (org-roam-db-query all-tags-query))
         (all-tags (mapcar #'car all-tags-result))
         (tag (completing-read "Tag: " all-tags)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags: \\(.*\\)$" nil t)
          (let ((existing-tags (match-string-no-properties 1)))
            (beginning-of-line)
            (delete-region (point) (line-end-position))
            (insert (format "#+filetags: %s%s:" existing-tags tag)))
        ;; No existing tags, search for title line to place new tags after
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:.*$" nil t)
            (progn
              (end-of-line)
              (insert (format "\n#+filetags: :%s:" tag)))
          (goto-char (point-min))
          (insert (format "#+filetags: :%s:\n" tag)))))))

(defun aero/org-set-tags ()
  "Set tag on current entry or file."
  (interactive)
  (if (org-before-first-heading-p)
      (aero/org-add-file-tag)
    (org-set-tags-command)))


;; Org-roam created and updated timestamps

(defun aero/org-roam-insert-created-property ()
  "Insert a :created: property for a new Org-roam node if it doesn't already have one."
  (interactive)
  (when (org-roam-file-p)
    (unless (org-entry-get (point-min) "created" t)
      (let ((creation-time (aero/org-roam-extract-timestamp-from-filepath
                            (buffer-file-name))))
        (when creation-time
          (save-excursion
            (goto-char (point-min))
            (org-set-property "created" creation-time)))))))

(defun aero/org-roam-extract-timestamp-from-filepath (filepath)
  "Extract timestamp from the Org-roam FILEPATH assuming it follows the default naming scheme."
  (let ((filename (file-name-nondirectory filepath)))
    (when (string-match "\\([0-9]\\{8\\}\\)\\([0-9]\\{4\\}\\)" filename)
      (let ((year (substring filename (match-beginning 1) (+ (match-beginning 1) 4)))
            (month (substring filename (+ (match-beginning 1) 4) (+ (match-beginning 1) 6)))
            (day (substring filename (+ (match-beginning 1) 6) (+ (match-beginning 1) 8)))
            (hour (substring filename (match-beginning 2) (+ (match-beginning 2) 2)))
            (minute (substring filename (+ (match-beginning 2) 2) (+ (match-beginning 2) 4))))
        (let ((time-struct (date-to-time (format "%s-%s-%sT%s:%s" year month day hour minute))))
          (format-time-string "[%Y-%m-%d %a %H:%M]" time-struct))))))

(defun aero/org-roam-insert-modified-property ()
  "Update the :modified: property for an Org-roam node upon saving."
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))  ; Ensure property is applied to the whole file
      (org-set-property "modified" (format-time-string "[%Y-%m-%d %a %H:%M]")))))


;; Roam

(package! org-roam
  (:repo "org-roam/org-roam" :files (:defaults "extensions/*"))
  :defer 1  ; don't load immediately, but soon after init

  :after (general org)

  :custom
  (org-roam-directory (expand-file-name "roam" aero/thornlog-path))
  (org-roam-mode-sections
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section))

  (org-id-locations-file (expand-file-name ".org-id-locations" aero-cache-dir))

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :immediate-finish t  ; skip capture buffer, just open the file
      :unnarrowed t)))

  (org-roam-node-display-template
   (concat "${title} " (propertize "${tags}" 'face 'org-tag)))

  :config
  (org-roam-db-autosync-mode)

  (add-hook 'before-save-hook #'aero/org-roam-insert-created-property)
  (add-hook 'before-save-hook #'aero/org-roam-insert-modified-property)

  (aero-leader-def
    "vf" 'org-roam-node-find
    "vF" 'org-roam-capture
    "vi" 'org-roam-node-insert
    "vc" '(org-id-get-create :wk "create org ID for node")
    "va" 'org-roam-alias-add
    "vr" 'org-roam-refile))

(package! consult-org-roam "jgru/consult-org-roam"
  :after (org-roam general)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (aero-leader-def
    "vb" 'consult-org-roam-backlinks
    "vB" 'consult-org-roam-backlinks-recursive
    "vl" 'consult-org-roam-forward-links
    "v'" 'consult-org-roam-search))


;;; EWW and elfeed

(package! eww :builtin
  :after (general evil ace-link)
  :commands (eww eww-browse-url eww-search-words browse-url-at-point)

  :preface
  (defmacro shr-display-block (tag)
    "Register TAG a paragraph (in CSS parlance \"display:block;\").

See https://developer.mozilla.org/en-US/docs/Glossary/Block-level_content"
    (let ((fname
           (intern (format "shr-tag-%s" tag)))
          (docstring
           (format "Render \"%s\" tag as paragraph." tag)))
      `(defun ,fname (dom)
         ,docstring
         (shr-ensure-paragraph)
         (shr-generic dom)
         (shr-ensure-paragraph))))

  (defun aero/set-eww-buffer-title ()
    "Rename eww mode buffer so the title of the page is displayed, making
     fake-tabbed-browsing easier"
    (let ((title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode)
        (if title
            (rename-buffer (concat "eww - " title) t)
          (rename-buffer "eww" t)))))

  (defun aero/wiki-news () (interactive)
         (eww-browse-url "https://en.wikipedia.org/wiki/Portal:Current_events"))

  (defun aero/xwidgets-search-ddg (&optional term)
    (interactive "sSearch DuckDuckGo: ")
    (xwidget-webkit-browse-url (format "https://duckduckgo.com/?q=%s" (or term "")) t))

  (defun aero/ace-link-eww-new-buffer ()
    "Call `ace-link-eww' but open in a new buffer.

This simply calls `ace-link-eww' with a fake double prefix, which is equivalent to the list containing 16."
    (interactive)
    (ace-link-eww '(16)))

  (defun aero/pocket-add-current-url ()
    "Add the URL currently visited to Pocket."
    (interactive)
    (when (require 'pocket-reader nil t)
      (let ((url (eww-current-url)))
        (if (pocket-lib-add-urls url)
            (message "Added: %s" url)
          (message "Failed to add to Pocket")))))

  :hook ((eww-mode . visual-line-mode)
         (eww-after-render . aero/set-eww-buffer-title))

  :custom
  ;; Open everything in eww, except for these few sites which just don't work in eww
  (browse-url-browser-function
   '((".*google.*maps.*" . browse-url-generic)
     ("docs.google.com" . browse-url-generic)
     ("*.atlassian.com" . browse-url-generic)
     ("*.atlassian.net" . browse-url-generic)
     ("github.com" . browse-url-generic)
     ("gitlab.com" . browse-url-generic)
     ("melpa.org" . browse-url-generic)
     ("zoom.us" . browse-url-generic)
     ("t.co" . browse-url-generic)
     ("twitter.com" . browse-url-generic)
     ("youtube.com" . browse-url-generic)
     ("*.reddit.com" . browse-url-generic)
     ("." . eww-browse-url)))

  ;; MacOS needs its hand held to find the binary
  (browse-url-generic-program (if (system-is-mac)
                                  "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"
                                "firefox"))

  (eww-search-prefix "https://lite.duckduckgo.com/lite?q=")
  (shr-max-width 90)
  (shr-indentation 2)
  (url-privacy-level 'high) ; don't send email nor last location

  :init
  (aero-leader-def "wbn" '(aero/wiki-news :wk "wikipedia news"))

  :config
  (evil-define-key 'normal eww-mode-map
    "SPC SPC" 'execute-extended-command
    "?" 'describe-mode
    "^" 'eww-up-url
    "u" 'eww-up-url
    "U" 'eww-top-url
    (kbd "<backspace>") 'eww-back-url
    "H" 'eww-back-url
    "L" 'eww-forward-url
    "&" 'eww-browse-with-external-browser
    "D" 'eww-download
    "o" 'eww
    "O" 'eww-open-in-new-buffer
    "p" 'pocket-reader-eww-add-link
    "P" 'aero/pocket-add-current-url
    "f" 'ace-link-eww
    "F" 'aero/ace-link-eww-new-buffer
    "m" 'eww-add-bookmark
    "R" 'eww-readable
    "r" 'eww-reload
    "gr" 'eww-reload
    "J" 'eww-buffer-show-next
    "K" 'eww-buffer-show-previous
    "T" 'eww-open-in-new-buffer
    "W" 'eww-copy-page-url
    "q" 'kill-current-buffer
    "Q" 'quit-window
    "go" 'eww
    "gf" 'eww-view-source
    "gc" 'url-cookie-list
    "gh" 'eww-list-histories
    "gb" 'eww-list-buffers
    "gt" 'eww-list-buffers)

  ;; viewing history
  (evil-set-initial-state 'eww-history-mode 'normal)
  (evil-define-key 'normal eww-history-mode-map
    (kbd "RET") 'eww-history-browse
    "q" 'quit-window)

  ;; viewing buffers
  (evil-set-initial-state 'eww-buffers-mode 'normal)
  (evil-define-key 'normal eww-buffers-mode-map
    "D" 'eww-buffer-kill
    (kbd "RET") 'eww-buffer-select
    "q" 'quit-window)

  ;; Handle display block elements
  (shr-display-block "article")
  (shr-display-block "aside")
  (shr-display-block "footer")
  (shr-display-block "header")
  (shr-display-block "nav")
  (shr-display-block "section")

  ;; bookmarks
  (evil-set-initial-state 'eww-bookmark-mode 'normal)
  (evil-define-key 'normal eww-bookmark-mode-map
    "D" 'eww-bookmark-kill
    "P" 'eww-bookmark-yank
    (kbd "RET") 'eww-bookmark-browse
    "q" 'quit-window))

;; Add some org-like features
(package! shrface (:host github :repo "chenyanming/shrface")
  :defer t
  :after (eww)
  :hook (eww-after-render . shrface-mode)
  :custom (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings))

;; Add syntax highlighting to HTML pre tags
(package! shr-tag-pre-highlight "xuchunyang/shr-tag-pre-highlight.el"
  :after (shr)
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(package! devdocs (:host github :repo "astoff/devdocs.el")
  :after (general)
  :commands (devdocs-lookup)
  :custom (devdocs-data-dir (expand-file-name "devdocs" aero-cache-dir))
  :init
  (aero-leader-def "hD" 'devdocs-lookup)
  (add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'typescript-mode-hook (lambda ()
                                    (setq-local devdocs-current-docs
                                                '("typescript" "rxjs" "angular" "javascript"))))
  (add-hook 'ng2-ts-mode-hook (lambda ()
                                (setq-local devdocs-current-docs
                                            '("typescript" "angular" "rxjs" "javascript" "html"))))
  (add-hook 'web-mode-hook (lambda ()
                             (setq-local devdocs-current-docs
                                         '("angular" "rxjs" "javascript" "html"))))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq-local devdocs-current-docs
                                             '("clojure~1.11")))))

(package! elfeed "skeeto/elfeed"
  :commands (elfeed elfeed-db-compact)
  :after (general evil)
  :custom
  (elfeed-search-title-max-width 120)
  (elfeed-db-directory aero/thornlog-elfeed-directory)
  (elfeed-search-filter "+unread")
  (elfeed-sort-order 'ascending)
  :config
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-set-initial-state 'elfeed-show-mode 'normal))

;; lets us use an elfeed.org file to manage our feeds and their tags.
(package! aero-elfeed-org :localpackage
  :config
  (when aero/thornlog-elfeed-org-file
    (setq aero-elfeed-org-file aero/thornlog-elfeed-org-file))
  (aero-elfeed-org))


;;; AI

(package! gptel "karthink/gptel"
  :after (general)
  :commands (gptel gptel-send gptel-menu)
  :custom
  (gptel-api-key openai-api-key)
  (gptel-model 'o1) ; default model when starting a buffer
  (gptel-use-header-line t)
  (gptel-display-buffer-action '(pop-to-buffer-same-window)) ; chat in same window
  (gptel-prompt-prefix-alist '((markdown-mode . "\n### ")
                               (org-mode . "-----\n*** ")
                               (text-mode . "------\n### ")))
  (gptel-response-prefix-alist '((markdown-mode . "\n")
                                 (org-mode . "-----\n")
                                 (text-mode . "------\n")))
  (gptel-directives
   '((default
       . "You are a large language model living in Emacs and a helpful, competent assistant. You do not have feelings and you do not apologize for anything. The user is a senior software engineer with limited time; you treat the user's time as precious, but you are not afraid to ask for clarification when needed. Respond concisely and cite sources for factual claims. NEVER explain code unless asked to do so, the explanation is a waste of time unless you are instructed to provide it. Do not add explanations or descriptions unless asked. Use Github-flavored Markdown for code snippets. When using Python, assume the user is using version 3.9 or newer. When using Typescript, assume the user is using version 4.8 or newer. When using SQL, use lowercase keywords. Do no use LaTeX.")
     (programming
      . "You are a large language model and a careful, competent programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")))

  :init
  (aero-leader-def
    "aic" 'gptel
    "ais" '(gptel-send :wk "send region or buffer to point")
    "aim" 'gptel-menu)

  (defun aero/gptel-send-buffer ()
    "If in gptel buffer, goto end and call gptel-send."
    (interactive)
    (when gptel-mode
      (save-excursion
        (goto-char (point-max))
        (call-interactively 'gptel-send))))

  (general-define-key
   :keymaps 'gptel-mode-map
   (kbd "C-<return>") 'aero/gptel-send-buffer)

  :config
  ;; Register Claude backend, but we don't set it as default because it hallucinates much more than
  ;; GPT.
  (when (and (boundp 'anthropic-api-key) anthropic-api-key)
    (gptel-make-anthropic "Claude" :stream t :key anthropic-api-key)))

;; Quick llm lookup at point or region. Uses posframe if its installed (prelude)
(package! gptel-quick "karthink/gptel-quick"
  :after (general)
  :commands (gptel-quick)
  :init
  (aero-leader-def "aiq" 'gptel-quick))

;; Works best with company-box, so we consider it a requirement
(package! copilot (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after (company-box general)
  :hook (prog-mode . copilot-mode)
  :custom (copilot-idle-delay 0.2) ; longer than default so it doesn't mess with typing
  :bind (:map copilot-mode-map
         ("C-<tab>" . copilot-accept-completion)
         ("C-c C-i" . copilot-accept-completion)
         ("C-c C-n" . copilot-next-completion)
         ("C-c C-p" . copilot-previous-completion)))


;;; Shell, Eshell

(package! xterm-color :auto
  :commands (xterm-color-filter)
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun aero/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'aero/advice-compilation-filter))


;; eshell

(package! eshell :builtin
  :after (general evil)
  :commands eshell
  :defines (evil-move-cursor-back
            eshell-save-history-on-exit
            eshell-history-size
            eshell-glob-case-insensitive
            eshell-ls-initial-args
            eshell-cmpl-dir-ignore
            eshell-visual-commands
            eshell-visual-subcommands)
  :functions (eshell-previous-input
              eshell-next-input)
  :config
  ;; Ensure eshell doesn't override these
  (define-key eshell-mode-map (kbd "M-h") #'windmove-left)
  (define-key eshell-mode-map (kbd "M-l") #'windmove-right)
  (define-key eshell-mode-map (kbd "M-p") #'eshell-previous-input)
  (define-key eshell-mode-map (kbd "M-n") #'eshell-next-input)
  (define-key eshell-mode-map (kbd "M-r") #'consult-history)

  (setq
   eshell-save-history-on-exit t
   eshell-buffer-maximum-lines 12000
   eshell-glob-case-insensitive t
   eshell-aliases-file (expand-file-name "eshell-alias" aero-etc-dir)
   eshell-history-size 350
   eshell-ls-initial-args "-lah"
   eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
   eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                            "ncftp" "pine" "tin" "trn" "elm" "vim"
                            "nmtui" "alsamixer" "htop" "el" "elinks"
                            "ssh" "nethack" "dtop" "dstat" "docker-compose")
   eshell-visual-subcommands '(("git" "log" "diff" "show"))
   eshell-cmpl-cycle-completions nil ; tab cycles the completion list
   eshell-buffer-maximum-lines 12000 ; auto truncate after 12k lines
   eshell-history-size 500 ; history size
   eshell-buffer-shorthand t ; buffer shorthand -> echo foo > #'buffer
   eshell-plain-echo-behavior t ; treat 'echo' like shell echo
   eshell-banner-message '(format "%s %s\n"
                                  (propertize (format " %s " (string-trim (buffer-name)))
                                              'face 'mode-line-highlight)
                                  (propertize (current-time-string)
                                              'face 'font-lock-keyword-face))
   eshell-scroll-to-bottom-on-input 'all
   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t  ; mimics zsh behavior
   completion-ignore-case t)

  ;; Enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  ;; Try to load in PATH
  (let ((default-directory (expand-file-name "~")))
    (setq eshell-path-env (getenv "PATH")))

  ;; doesn't handle less too well
  ;; (setenv "PAGER" "cat")
  (setenv "PAGER" "bat")
  (setenv "TERM" "xterm-256color")

  ;; Remove hscroll-margin in shells, otherwise you get jumpiness when the
  ;; cursor comes close to the left/right edges of the window.
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local hscroll-margin 0
                                   evil-move-cursor-back nil)))

  ;; Use tab to cycle completions
  (add-hook 'eshell-mode-hook (lambda () (setq-local pcomplete-cycle-completions nil)))

  (defalias 'eshell/emacs 'find-file)

  (defun eshell/e (pattern)
    (if (stringp pattern)
        (find-file pattern)
      (mapc #'find-file (mapcar #'expand-file-name pattern))))

  (defun eshell/rmdanglingdockers ()
    (let ((dangling-images (shell-command-to-string "docker images -f \"dangling=true\" -q")))
      (if (string-empty-p dangling-images)
          (message "No dangling images found.")
        (eshell-command (concat "docker rmi " dangling-images)))))

  (defun eshell/dockerkillorphans ()
    (let ((orphan-volumes (shell-command-to-string "docker volume ls -qf dangling=true")))
      (if (string-empty-p orphan-volumes)
          (message "No orphan volumes found.")
        (eshell-command (concat "docker volume rm " orphan-volumes)))))

  (defun eshell/dockercleanup ()
    (eshell/dockerkillorphans)
    (eshell/rmdanglingdockers))

  ;; So the history vars are defined
  (require 'em-hist)
  (when (boundp 'eshell-save-history-on-exit)
    ;; Don't ask, just save
    (setq eshell-save-history-on-exit t))

  (eval-after-load 'esh-opt
		'(progn
       (require 'em-cmpl)
       (require 'em-prompt)
       (require 'em-term))))

(package! eshell-prompt-extras :auto
  :after (eshell)
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-multiline-with-status)))

(package! eshell-syntax-highlighting :auto
  :after eshell-mode
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(package! esh-help :auto
  :after eshell
  :config (setup-esh-help-eldoc))

;; Provides overlay suggestions in eshell. Use M-f to insert the next suggested word
(package! capf-autosuggest "emacs-straight/capf-autosuggest"
  :hook (eshell-mode . capf-autosuggest-mode))


;; shell scripting

(package! sh-script :builtin :defer t
  :mode ("\\.\\(sh\\|bash\\|zsh\\|zsh-theme\\)\\'" . sh-mode)
  :config
  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))


;;; Lisp

(package! common-lisp-mode :builtin
  :mode "\\(Lakefile|\\.\\(cl|lisp\\)\\)\\'")

(package! slime :auto
  :commands slime
  :init
  (setq-default
   inferior-lisp-program "ecl"
   slime-contribs '(slime-fancy))
  ;; Load SBCL faster by using preset socket and POSIX shit.
  ;; NOTE: this requires some set-up beforehand in the SBCL REPL:
  ;;   * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;;   * (save-lisp-and-die "sbcl.core-for-slime")
  (defvar slime-lisp-implementations)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))
          (ecl ("ecl")))))

(defun indent-defun ()
  "Indent current defun"
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

;; redefined to turn this:
;; (:foo bar
;;       :spam ham)
;; into this:
;; (:foo bar
;;  :spam ham)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine if the arguments of
a Lisp function call should be indented specially. INDENT-POINT is the position
at which the line being indented begins. Point is located at the point to indent
under (for default indentation); STATE is the `parse-partial-sexp' state for
that position. If the current line is in a call to a Lisp function that has a
non-nil property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent. The property value can be:

* `defun', meaning indent `defun'-style \(this is also the case if there is no
  property and the function has a name that begins with \"def\", and three or
  more arguments);

* an integer N, meaning indent the first N arguments specially (like ordinary
  function arguments), and then indent any further arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments that it
  itself received.

This function returns either the indentation to use, or nil if the Lisp function
does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (defvar calculate-lisp-indent-last-sexp)
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same line as
         ;; calculate-lisp-indent-last-sexp. Note that first thing on that line
         ;; has to be complete sexp since we are inside the innermost containing
         ;; sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'common-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))


;; Clojure

(package! clojure-mode :auto :mode "\\.\\(cljs?\\|cljs.*\\|edn\\|boot\\)\\'")
(package! cider :auto
  :hook (clojure-mode . cider-mode)
  :after (clojure-mode general)
  :commands (cider
             cider-jack-in)
  :custom
  (cider-show-error-buffer t)
  (cider-repl-history-file (expand-file-name "cider-history" aero-cache-dir))
  :config
  (aero-mode-leader-def
    :keymaps '(clojure-mode-map cider-mode-map)
    "c" '(:ignore t :wk "cider")
    "c RET" 'cider-run
    "cj" 'cider-jack-in
    "c'" 'cider-switch-to-repl-buffer
    "c," 'cider-pop-back
    "cQ" 'cider-quit
    "cl" '(:ignore t :wk "load")
    "clb" 'cider-load-buffer
    "clf" 'cider-load-file
    "cld" '(cider-load-all-files :wk "load directory")
    "cR" 'cider-ns-refresh
    "ch" '(:ignore t :wk "help")
    "chd" 'cider-doc
    "chj" 'cider-javadoc
    "chc" 'cider-clojuredocs
    "chC" 'cider-clojuredocs-web
    "cha" 'cider-apropos
    "chD" 'cider-apropos-documentation
    "ct" '(:ignore t :wk "test")
    "ctt" 'cider-test-run-test
    "ctr" 'cider-test-rerun-test
    "ctn" 'cider-test-run-ns-tests
    "ctp" 'cider-test-run-project-tests
    "ctf" 'cider-test-rerun-failed-tests
    "ctp" 'cider-test-show-report
    "cb" 'cider-load-buffer-and-switch-to-repl-buffer
    "cd" 'cider-eval-defun-at-point
    "cs" 'cider-eval-sexp-at-point
    "cr" 'cider-eval-region
    "cm" '(:ignore t :wk "macro expand")
    "cmm" 'cider-macroexpand-1
    "cma" 'cider-macroexpand-all
    "cN" 'cider-eval-ns-form
    "ce" '(:ignore t :wk "echo")
    "cee" '(cider-eval-last-sexp :wk "echo last sexp")
    "cer" '(cider-eval-last-sexp-to-repl :wk "eval last sexp to repl")
    "cep" '(cider-pprint-eval-last-sexp :wk "pprint last sexp"))

  (with-eval-after-load 'lsp-mode
    (aero-mode-leader-def
      :keymaps 'clojure-mode-map
      "r" '(:ignore t :wk "refactor")
      "rt" '(:ignore t :wk "thread")
      "rtt" 'lsp-clojure-thread-first
      "rtT" 'lsp-clojure-thread-first-all
      "rtl" 'lsp-clojure-thread-last
      "rtL" 'lsp-clojure-thread-last-all
      "rL" 'lsp-clojure-add-missing-libspec
      "rC" 'lsp-clojure-cycle-coll
      "rl" '(:ignore t :wk "let")
      "rle" 'lsp-clojure-expand-let
      "rli" 'lsp-clojure-introduce-let
      "rlm" 'lsp-clojure-move-to-let
      "rU" 'lsp-clojure-unwind-all
      "rp" 'lsp-clojure-cycle-privacy
      "re" 'lsp-clojure-extract-function
      "rs" 'lsp-clojure-inline-symbol)))


;; Elisp-specific

;; Linting for Emacs packages
(package! package-lint "purcell/package-lint"
  :commands (package-lint-current-buffer))

(package! elisp-autofmt :auto
  :commands (elisp-autofmt-buffer
             elisp-autofmt-region)
  :custom
  (elisp-autofmt-cache-directory
   (expand-file-name "elisp-autofmt-cache" aero-cache-dir)))

(package! el2md (:host gitlab :repo "thornjad/el2md")
  :after (general)
  :commands (el2md-write-readme
             el2md-view-buffer
             el2md-write-file)
  :init
  (aero-mode-leader-def
    :keymaps 'emacs-lisp-mode-map
    "m" '(:ignore t :wk "el2md")
    "mr" 'el2md-write-readme
    "mv" 'el2md-view-buffer
    "mw" 'el2md-write-file))


;;; Yarn-lock derived mode

(defvar yarn-lock-mode-syntax-table
  (let ((syntable (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntable)
    (modify-syntax-entry ?\n ">" syntable)
    (modify-syntax-entry ?\" "\"" syntable)
    syntable))

(defvar yarn-lock-mode-package-re "\\(^\\|,\\s-\\)\\([a-zA-Z-_0-9]+\\)@")
(defvar yarn-lock-mode-dependencies-re "\\s-\\{4,\\}\\([a-zA-Z-_0-9]+\\)\\s-")
(defvar yarn-lock-mode-attributes-re
  (regexp-opt '("version" "resolved" "dependencies" "integrity")))
(defvar yarn-lock-mode-font-lock-defaults
  `((,yarn-lock-mode-attributes-re . '((t :inherit font-lock-builtin-face)))
    (,yarn-lock-mode-package-re . (2 '((t :inherit bold)) t)) ;; Direct deps
    (,yarn-lock-mode-dependencies-re . (1 '((t :inherit bold)) t)) ;; Dep of another dep (nested)
    ))
(define-derived-mode yarn-lock-mode text-mode "Yarn Lock"
  "Simple mode for yarn.lock."
  :syntax-table yarn-lock-mode-syntax-table
  (setq font-lock-defaults '(yarn-lock-mode-font-lock-defaults)
        buffer-read-only t))


(provide 'aero-layers)
;;; aero-layers.el ends here
