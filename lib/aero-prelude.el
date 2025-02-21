;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer, setting up the foundation of the rest of the config
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
;; After init.el, this file is the core driver of Aero. It sets up ubiquitous packages and the
;; primary keybindings.
;;
;; The file name of "prelude" has nothing to do with the excellent Emacs configuration distribution
;; of the same name. Instead, the name alludes to the fact that the configuration here comes before
;; the rest, and in many cases is required by other packages.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Load our utilities
(require 'aero-lib)


;;; Set up core packages and prereqs

;; Require early so we can use this whenever
(require 'aero-package (expand-file-name "lib/aero-package.el" user-emacs-directory))

;; Set up automatic compilation for everything past this point
(package! compile-angel "jamescherti/compile-angel.el"
  :demand t
  :hook (emacs-lisp-mode-hook . compile-angel-on-save-local-mode)

  :custom
  (compile-angel-verbose t)
  (compile-angel-enable-byte-compile nil) ; only native compile

  :config
  ;; Exclude these files
  (with-eval-after-load "savehist" (push (concat "/" (file-name-nondirectory savehist-file))
                                         compile-angel-excluded-files))
  (with-eval-after-load "recentf" (push (concat "/" (file-name-nondirectory recentf-save-file))
                                        compile-angel-excluded-files))
  (with-eval-after-load "cus-edit" (push (concat "/" (file-name-nondirectory custom-file))
                                         compile-angel-excluded-files))

  (compile-angel-on-load-mode))

;; The ELPA keyring sometimes gets screwed up, this fixes it
(package! gnu-elpa-keyring-update :auto)

;; Requirements for lib
(package! dash "magnars/dash.el")
(package! memo (:host gitlab :repo "thornjad/emacs-memo" :branch "main"))
(package! async "jwiegley/emacs-async" :commands (async-save))
(package! popup "auto-complete/popup-el")
(package! spinner "Malabarba/spinner.el")

;; used by gptel-quick and available for other stuff
(package! posframe "tumashu/posframe" :defer 1)

;; Used by eglot, dape, copilot, etc
(package! jsonrpc :builtin
  :config
  ;; Don't waste time logging events
  (fset #'jsonrpc--log-event #'ignore))

;; Mostly only required for MacOS, we need to grab environment variables from the default shell.
(package! exec-path-from-shell "purcell/exec-path-from-shell"
  :when (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "PATH" "MANPATH" "INFOPATH" "LSP_USE_PLISTS" "HOMEBREW_PREFIX" "HOMEBREW_CELLAR" "HOMEBREW_REPOSITORY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Faster than grep, but requires ripgrep to be installed locally
(package! ripgrep "nlamirault/ripgrep.el" :defer 3)

;; Make files executable if the first file has a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;; Location definitions

(defvar aero/documents-path (expand-file-name "~/Documents/"))
(defvar aero/thornlog-path (expand-file-name "thornlog/" aero/documents-path))
(defvar aero/roam-path (expand-file-name "roam/" aero/thornlog-path))
(defvar aero/thornlog-archive-file (expand-file-name "archive/archive.org" aero/thornlog-path))
(defvar aero/thornlog-elfeed-directory (expand-file-name "elfeed/" aero/documents-path)
  "The directory where elfeed will store its database and other files.")
(defvar aero/thornlog-elfeed-org-file (expand-file-name "rss_feeds.org" aero/roam-path))


;;; Keybindings

(package! which-key "justbur/emacs-which-key"
  :hook (on-first-input . which-key-mode)
  :defines which-key-mode
  :config
  (which-key-mode)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")))

(package! general "noctuid/general.el"
  :functions (general-define-key aero-leader-def aero-mode-leader-def)
  :init
  (setq-default general-override-states
                '(insert hybrid normal visual motion operator replace))

  ;; Most bindings will fall under this leader key, so we make a handy macro.
  (general-create-definer aero-leader-def
    :states '(normal visual emacs motion)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  ;; Mode-leader lets us put keybindings only in specific modes (usually major modes).
  (general-create-definer aero-mode-leader-def
    :states '(normal visual emacs motion)
    :prefix "SPC ,")

  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil)

  ;; Main configuration

  (general-def
    ;; Emacs chose ^? for the help system for some despicable reason. Fuck that.
    (kbd "C-h") 'delete-backward-char
    (kbd "C-w") 'aero/smarter-backward-kill-word
    (kbd "C-TAB") 'insert-tab
    (kbd "M-TAB") 'aero/alternate-buffer
    (kbd "C-RET") 'aero/browse-url-open)

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fW" 'evil-write-all
   "w/" '(evil-window-vsplit :wk "split vertical")
   "w-" '(evil-window-split :wk "split horizontal")
   "w2" 'aero/layout-two-columns
   "w3" 'aero/layout-three-columns
   "cm" 'evil-make)

  (global-set-key [remap keyboard-quit] #'aero/keyboard-quit-context)

  ;; mode-specific overrides
  (general-define-key
   :states '(normal insert motion)
   :keymaps 'override
   :prefix ","
   "" nil)

  (general-define-key
   :states '(normal insert motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil

   ;; independent keys
   "SPC" 'execute-extended-command
   "TAB" '(aero/alternate-buffer :wk "alternate buffer")
   (kbd "ESC") 'keyboard-quit
   (kbd "C-g") 'keyboard-quit
   (kbd "<pause>") 'keyboard-quit
   "'" 'eshell
   "\"" '(aero/eshell-new :wk "eshell-new")
   ":" 'eval-expression
   ";" 'aero/comment-dwim
   "!" 'shell-command
   "=" 'quick-calc

   "," '(:ignore t :wk "mode") ; reserved for mode-specific

   "e" '(:ignore t :wk "errors")
   "ed" 'toggle-debug-on-error
   "eq" 'toggle-debug-on-quit

   "T TAB" 'tab-recent
   "T" '(:ignore t :wk "tab")
   "Tn" 'tab-next
   "Tp" 'tab-previous
   "Tk" 'tab-close
   "T," 'tab-rename
   "Tc" '(tab-new :wk "create tab")
   "Tb" 'switch-to-buffer-other-tab
   "Tf" 'find-file-other-tab
   "Ts" '(tab-duplicate :wk "tab duplicate split")
   "Tu" 'tab-undo

   "U" 'universal-argument

   "a" '(:ignore t :wk "applications")
   "ai" '(:ignore t :wk "AI functions")

   "b" '(:ignore t :wk "buffers")
   "bs" 'switch-to-scratch-buffer
   "bS" 'switch-to-new-scratch-buffer
   "bd" 'kill-current-buffer
   "bi" 'indent-buffer
   "bl" 'ibuffer
   "bm" 'switch-to-messages-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "br" '(aero/reopen-file-at-buffer :wk "buffer replace")
   "bR" '(revert-buffer-quick :wk "buffer revert")
   "bw" '(whitespace-mode :wk "whitespace")
   "bx" 'kill-buffer-and-window

   "n" '(:ignore t :wk "narrow")
   "nn" 'narrow-to-region
   "np" 'narrow-to-page
   "nw" 'widen
   "nd" 'narrow-to-defun

   "c" '(:ignore t :wk "compile")
   "ct" 'aero/tail-compilation-buffer
   "ci" '(ielm :wk "ielm repl")
   "cc" 'compile
   "ce" '(:ignore t :wk "elisp")
   "ceb" 'eval-buffer
   "ced" 'eval-defun
   "cer" 'eval-region
   "ck" 'kill-compilation
   "cr" 'recompile

   "f" '(:ignore t :wk "files")
   "ff" 'find-file
   "fc" 'aero/copy-file-relative-to-project
   "fD" '(aero/delete-this-file :wk "delete this file")
   "fR" '(aero/rename-this-file-and-buffer :wk "rename this file")
   "fo" '(:ignore t :wk "open special files")
   "fot" '(:ignore t :wk "thornlog")
   "fota" 'aero/open-agenda-file
   "fott" '(aero/thornlog-todo :wk "thornlog todo")
   "fotl" '(aero/thornlog-log :wk "thornlog log")
   "fotd" '(aero/thornlog-dir :wk "thornlog all")
   "fw" '(save-buffer :wk "write buffer")
   "fh" '(aero/toggle-angular-component-file :wk "toggle angular component file")

   "g" '(:ignore t :wk "git")
   "gf" '(:ignore t :wk "files")

   "h" '(:ignore t :wk "help/manual")
   "hI" 'info-apropos
   "hM" 'woman
   "hd" '(:ignore t :wk "describe")
   "hdF" 'describe-face
   "hdb" 'describe-bindings
   "hdM" 'describe-mode
   "hdK" 'describe-keymap
   "hdC" 'describe-char
   "hdp" 'describe-package
   "hdi" '(emacs-index-search :wk "search emacs manual")
   "hdl" '(find-library :wk "describe library")
   "hi" 'info
   "hm" 'man
   "hw" '(:ignore t :wk "which-key")
   "hwm" '(which-key-show-major-mode :wk "major mode map")

   "j" '(:ignore t :wk "jump")
   "l" '(:ignore t :wk "lsp")

   "m" '(:ignore t :wk "mode")
   "m" '(tmm-menubar :wk "Context menu")

   "o" '(:ignore t :wk "org / outline")
   "oh" '(outline-hide-body :wk "hide all")
   "oS" '(outline-show-all :wk "show all")

   "p" '(:ignore t :wk "project")
   "pr" '(xref-find-definitions :wk "find ref")
   "ps" '(:ignore t :wk "spelling")

   "r" '(:ignore t :wk "xref")
   "rf" 'xref-find-definitions
   "rF" 'xref-find-definitions-other-window
   "rp" 'xref-go-back
   "rn" 'xref-go-forward
   "ra" 'xref-find-apropos
   "rr" 'xref-find-references

   "q" '(:ignore t :wk "quoted insert")
   "ql" 'insert-lambda
   "qq" 'quoted-insert
   "qp" 'aero/insert-pdb
   "qu" 'insert-char

   "s" '(:ignore t :wk "sexp")

   "t" '(:ignore t :wk "tabs/text")
   "td" 'dictionary-lookup-definition
   "tD" 'downcase-dwim
   "tU" 'upcase-dwim
   "tf" 'fill-paragraph
   "tF" 'aero/fill-to-80
   "tn" '(:ignore t :wk "number")
   "tnd" 'decrement-number-at-point
   "tni" 'increment-number-at-point
   "ts" 'sort-lines

   "u" 'undo-tree-visualize

   "w" '(:ignore t :wk "window/web")
   "w=" 'balance-windows
   "wB" '(aero/switch-to-minibuffer-window :wk "switch to minibuffer")
   "ws" '(eww-search-words :which-key "web search")
   "ww" 'eww
   "wb" '(:ignore t :wk "browse")
   "wbb" 'eww-list-buffers
   "wbh" 'eww-list-histories
   "wbm" 'eww-list-bookmarks
   "wbp" 'browse-url-at-point
   "wp" 'browse-url-at-point
   "wc" 'aero/toggle-compilation-buffer
   "wd" 'delete-window
   "wh" 'windmove-left
   "wi" 'minimize-window
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wm" 'maximize-window
   "wo" 'aero/browse-url-open
   "w{" 'shrink-window
   "w}" 'enlarge-window

   "z" 'repeat))

(package! casual "kickingvegas/casual"
	:after (dired)
  :defines (casual-editkit-main-tmenu
            casual-dired-tmenu
            casual-dired-sort-by-tmenu
            casual-dired-search-replace-tmenu)
  :bind (("C-o" . #'casual-editkit-main-tmenu)

         :map dired-mode-map
         ("C-o" . #'casual-dired-tmenu)
         ("s" . #'casual-dired-sort-by-tmenu)
         ("/" . #'casual-dired-search-replace-tmenu)))


;;; Evil

(package! evil
  (:host github
   :repo "emacs-evil/evil"
   :files (:defaults
           "doc/build/texinfo/evil.texi"
           (:exclude "evil-test-helpers.el")))
  :init
  ;; Need to be in init because of something in the way the "want" variables are used
  (setq evil-want-keybinding nil ; handled by evil-collection
        ;; to change undo-system without restart, use SPC-: `evil-set-undo-system'
        evil-undo-system 'undo-tree
        evil-want-fine-undo t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t
        evil-search-module 'isearch)

  :config

  ;; Free these up for other bindings, they're not useful anyway
  (define-key evil-motion-state-map " " nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)

  ;; default states
  (setq evil-default-state 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)

  ;; Make movement keys work like they should by remapping next to next-visual, etc.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Ensure horizontal movement doesn't cross to the next/previous line
  (setq-default evil-cross-lines nil)

  ;; Undo in region
  (define-key evil-visual-state-map (kbd "u") 'undo)

  ;; By default, these two operate on half pages, but I prefer the smaller jump
  (defun aero/scroll-quarter-page-down ()
    (interactive)
    (evil-scroll-down (/ (window-body-height) 4)))
  (defun aero/scroll-quarter-page ()
    (interactive)
    (evil-scroll-up (/ (window-body-height) 4)))
  (evil-define-key nil global-map (kbd "C-u") #'aero/scroll-quarter-page-up)
  (evil-define-key nil global-map (kbd "C-d") #'aero/scroll-quarter-page-down)

  ;; Define vig and vag, etc. to look for all paren types
  (defun aero/evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
    (let* ((parens '("()" "[]" "{}" "<>"))
           range
           found-range)
      (dolist (p parens)
        (condition-case nil
            (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
          (error nil))
        (when range
          (cond
           (found-range
            (when (< (- (nth 1 range) (nth 0 range))
                     (- (nth 1 found-range) (nth 0 found-range)))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))
  (evil-define-text-object aero/evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (aero/evil-paren-range count beg end type t))
  (evil-define-text-object aero/evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (aero/evil-paren-range count beg end type nil))
  (define-key evil-inner-text-objects-map "g" #'aero/evil-inner-paren)
  (define-key evil-outer-text-objects-map "g" #'aero/evil-a-paren)

  ;; Very useful, in visual mode, use < and > to indent/unindent the line(s)
  (defun aero/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (defun aero/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-key 'visual global-map (kbd ">") 'aero/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'aero/evil-shift-left)

  ;; :q should kill the current buffer rather than quitting Emacs entirely
  (evil-ex-define-cmd "q" 'kill-current-buffer)

  ;; Unless I'm mistaken, there's no Evil backward equivalent to "e", so we'll invent it.
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-e") #'evil-backward-word-end)
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-M-e") #'evil-backward-WORD-end)

  ;; Useful for pasting into the minibuffer where Evil modes usually don't properly function
  (evil-define-key '(insert) global-map (kbd "C-y") #'evil-paste-after)
  (evil-define-key '(insert) global-map (kbd "C-S-y") #'evil-paste-before)

  ;; Run macro in register q
  (evil-define-key 'normal 'global "Q" "@q")
  (evil-define-key 'visual 'global
    ;; run macro in register q on region
    "Q" (kbd ":norm @q RET")
    ;; repeat on region
    "." (kbd ":norm . RET"))

  ;; activate
  (evil-mode +1))

;; Provides defaults for many modes which evil proper overlooks
(package! evil-collection (:repo "emacs-evil/evil-collection" :files (:defaults "modes"))
  :after evil
  :config (evil-collection-init))

;; allows % to jump matching tags
(package! evil-matchit "redguardtoo/evil-matchit"
  :defer 5
  :after evil
  :defines global-evil-matchit-mode
  :config (global-evil-matchit-mode 1))


;;; Treesitter

;; Automatically install treesitter grammars when missing
(package! treesit-auto "renzmann/treesit-auto"
  :when (treesitterp)
  :custom
  (treesit-auto-install 'prompt)

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Python is not playing nicely, so we'll pin it to a working version
  (defvar aero/python-treesit-auto-recipe
    (make-treesit-auto-recipe
     :lang 'python
     :ts-mode 'python-ts-mode
     :remap 'python-mode
     :url "https://github.com/tree-sitter/tree-sitter-python"
     :ext "\\.py[iw]?\\'"
     :revision "v0.21.0")
    "Recipe for libtree-sitter-python.dylib")
  (add-to-list 'treesit-auto-recipe-list aero/python-treesit-auto-recipe)

  (global-treesit-auto-mode +1))

;; Provide selection of functions
(package! evil-textobj-tree-sitter
  (:repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries" "treesit-queries"))
  :when (treesitterp)
  :after (evil)
  :config
  ;; vaf, select function outer
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; vif, select inner funciton
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))


;;; completion and navigation

(package! vertico "minad/vertico"
  :init (vertico-mode)
  :custom
  (vertico-cycle t)  ; enable wrap
  :config
  (defun aero/vertico-directory-up-maybe ()
    "Go up a directory if completing a file name, otherwise delete char."
    (interactive)
    (if (and (eq (char-before) ?/)
             (minibufferp)
             minibuffer-completing-file-name)
        (vertico-directory-up)
      (delete-char -1)))
  (define-key vertico-map (kbd "DEL") #'aero/vertico-directory-up-maybe))

(package! marginalia "minad/marginalia"
  :init (marginalia-mode))

;; Orderless completion style: space-separated chunks to match in any order
(package! orderless "oantolin/orderless"
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

(package! consult "minad/consult"
  :after (general evil orderless)
  :commands (consult-line
             consult-buffer
             consult-outline
             consult-imenu
             consult-flymake
             consult-theme
             consult-ripgrep)
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key '(:debounce 0.4 any))

  :init
  (aero-leader-def
    "/" 'consult-line
    "bb" 'consult-buffer
    "jo" 'consult-outline
    "ji" 'consult-imenu
    "je" 'consult-flymake
    "ja" 'consult-org-agenda
    "jh" 'consult-org-heading
    "p/" 'consult-ripgrep
    "Et" 'consult-theme
    "j'" 'consult-mark)

  :config
  ;; Support jumping to eshell prompts with consult-outline
  (add-hook 'eshell-mode-hook (lambda () (setq outline-regexp eshell-prompt-regexp)))

  ;; Use Orderless to compile the regexp for consult-ripgrep
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  (defun consult--with-orderless (&rest args)
    "Use Orderless to compile the regexp for consult-ripgrep."
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  (defun aero/consult-line-isearch-history (&rest _)
    "Add latest `consult-line' search pattern to the isearch history.

This allows n and N to continue the search after `consult-line' exits.

Note this only supports the first search term when using orderless syntax."
    (when (and (bound-and-true-p evil-mode)
               (eq evil-search-module 'isearch)
               consult--line-history)
      (let* ((pattern (car consult--line-history))
             (pattern (car (split-string pattern)))
             (regexp (if (string-prefix-p "\\_" pattern)
                         (substring pattern 2)
                       pattern)))
        (add-to-history 'regexp-search-ring regexp)
        (setq evil-ex-search-direction 'forward))))
  (advice-add #'consult-line :after #'aero/consult-line-isearch-history)

  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple'.
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Enhances `execute-extended-command' by showing recently used commands and keyboard shortcuts
(package! amx "DarwinAwardWinner/amx"
  :defer 1
  :init (amx-mode 1))

(package! yasnippet "joaotavora/yasnippet"
  :custom
  (yas-installed-snippets-dir aero-snippets-dir)
  :config
  (yas-global-mode 1))

(package! consult-yasnippet "mohkale/consult-yasnippet"
  :after (consult yasnippet)
  :config
  (aero-leader-def
    "y" 'consult-yasnippet))

(package! recentf :builtin
  :defer 1
  ;; Doesn't seem like indent activates properly for me without this intervention. Here we move it
  ;; to a known cache file and set up an auto-save every 5 minutes.
  :defines (recentf-mode)
  :preface
  (defun aero/recentf-save-list-quiet ()
    "Wrapper for `recentf-save-list' with no message."
    (let ((inhibit-message t))
      (recentf-save-list)))
  :custom
  (recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
  (recentf-max-saved-items 500)
  :config
  (recentf-mode 1)
  
  ;; Would be a great place for `aero/advice-no-message' but there's no need to hide messaging if
  ;; recentf saves for some other reason. Here, we run it regularly so we don't care about the
  ;; constant messaging.
  (run-at-time 60 (* 5 60) #'aero/recentf-save-list-quiet))

;; Add support for icon insertion, and use as a lib in other packages
(package! all-the-icons (:repo "domtronn/all-the-icons.el" :files (:defaults "data"))
  :after (general)
  :defer 1
  :when (display-graphic-p)
  :config (aero-leader-def "qi" 'all-the-icons-insert))

;; visual navigation utility
(package! avy "abo-abo/avy"
  :after (general)
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :wk "jump to line")
   "jc" '(avy-goto-char :wk "jump to char")
   "jj" '(avy-goto-char :wk "jump to char")
   "jw" '(avy-goto-word-1 :wk "jump to word")))

;; jump to search results in eww
(package! ace-link "abo-abo/ace-link"
  :after (avy eww)
  :functions (ace-link-setup-default)
  :config (ace-link-setup-default))


;;; Project

(package! project :builtin
  :after (general)

  :preface
  (defun aero/project-root-override (dir)
    "Find DIR's project root by searching for a '.project.el' file.

If this file exists, it marks the project root. For convenient compatibility with Projectile, '.projectile' is also considered a project root marker.

https://jmthornton.net/blog/p/emacs-project-override"
    (let ((root (or (locate-dominating-file dir ".project.el")
                    (locate-dominating-file dir ".projectile")))
          (backend (ignore-errors (vc-responsible-backend dir))))
      (when root (if (version<= emacs-version "28")
                     (cons 'vc root)
                   (list 'vc backend root)))))

  (defun aero/project-switch-magit ()
    "Call magit-status on the project being switched to."
    (interactive)
    (magit-status project-current-directory-override))

  :custom
  (project-vc-ignores '("node_modules/" "straight/" "target/")) ; globally ignored
  (project-vc-extra-root-markers '(".project.el" ".projectile" ".git"))
  (project-compilation-buffer-name-function #'project-prefixed-buffer-name)

  :config
  ;; Note that we cannot use :hook here because `project-find-functions' doesn't end in "-hook", and
  ;; we can't use this in :init because it won't be defined yet.
  (add-hook 'project-find-functions #'aero/project-root-override)

  ;; Set our own list of actions on `project-switch-project'
  (setq project-switch-commands '((project-find-file "Find file" "f")
                                  (aero/project-switch-magit "Magit status" "g")
                                  (project-eshell "Eshell" "e")
                                  (project-compile "Compile" "c")
                                  (project-find-dir "Find directory" "d")
                                  (project-find-regexp "Find regexp" "r")
                                  (project-any-command "Any command" "a")))

  (aero-leader-def
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "p:" 'project-shell-command
    "p&" 'project-async-shell-command
    "p'" 'project-eshell
    "p\"" 'aero/project-eshell-new
    "p`" 'project-shell
    "p%" 'project-query-replace-regexp
    "cp" 'project-compile))


;;; Code navigation

;; Gives us the M-n and M-p symbol-following ability
(package! smartscan "mickeynp/smart-scan"
  :hook (prog-mode . smartscan-mode)
  :config
  (advice-add 'smartscan-symbol-go-forward :around #'aero/advice-disable-subword)
  (advice-add 'smartscan-symbol-go-backward :around #'aero/advice-disable-subword))

(package! undo-tree "apchamberlain/undo-tree.el"
  :custom
  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree/" aero-cache-dir))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)

  :config
  (global-undo-tree-mode +1)
  ;; enable in non-file buffers too
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))

(package! winner :builtin
  :after (general)
  :defines winner-boring-buffers
  :config
  ;; list of buffers that winner-undo won't restore
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  (winner-mode 1)
  (aero-leader-def
    "wu" 'winner-undo
    "wU" 'winner-redo))

;; Jump to windows by number. 1 is the upper-left-most
(package! winum "deb0ch/emacs-winum"
  :defer 5
  :after (general which-key)
  :init
  (winum-mode)
  :config
  (aero-leader-def
    "1" '(winum-select-window-1 :wk "window-1")
    "2" '(winum-select-window-2 :wk "window-2")
    "3" '(winum-select-window-3 :wk "window-3")
    "4" '(winum-select-window-4 :wk "window-4")
    "5" '(winum-select-window-5 :wk "window-5")
    "6" '(winum-select-window-6 :wk "window-6")
    "7" '(winum-select-window-7 :wk "window-7")
    "8" '(winum-select-window-8 :wk "window-8")
    "9" '(winum-select-window-9 :wk "window-9")))


;;; Company completions

(package! company
  (:repo "company-mode/company-mode"
   :files (:defaults "icons" ("images/small" "doc/images/small/*.png")))
  :after (evil)
  :hook ((prog-mode . company-mode)
         (company-mode-hook . evil-normalize-keymaps))
  :init
  (setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15
        company-tooltip-margin 2
        company-require-match nil
        company-show-numbers t
        company-tooltip-align-annotations t
        company-dabbrev-other-buffers t ; only look in open buffers with same major mode
        company-global-modes '(not
                               erc-mode message-mode help-mode gud-mode vterm-mode))
  :config
  ;; Wait until it's defined, then disable preview after point
  (setq company-frontends (delq 'company-preview-if-just-one-frontend company-frontends)))

;; Move commonly-used completions to the top
(package! company-prescient
  (:host github
   :repo "radian-software/prescient.el"
   :files ("company-prescient.el"))
  :after (company)
  :hook (company-mode . company-prescient-mode)
  :custom (prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir))
  :config (prescient-persist-mode +1))

;; Better popup interface for company
(package! company-box
  (:repo "sebastiencs/company-box" :files (:defaults "images"))
  :hook (company-mode . company-box-mode))


;;; LSP

(package! eglot :builtin
  :hook ((python-mode
          python-ts-mode
          clojure-mode
          typescript-mode
          typescript-ts-mode
          js-mode
          js-ts-mode)
         . eglot-ensure)
  :after (general project)

  :custom
  (eglot-confirm-server-initiated-edits nil) ; don't ask to edit file immediately after I told it to
  (eglot-autoshutdown t) ; shutdown server after killing last managed buffer
  (eglot-events-buffer-size 0) ; disable event logging
  (eglot-send-changes-idle-time 0.75)
  ;; LSP highlighting is ridiculously slow, we use highlight-thing instead
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))

  :config
  ;; Individual server configuration
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                                    ;; :pyflakes (:enabled :json-false)
                                    :pyls_mypy (:enabled t
                                                :live_mode :json-false)
                                    :pyls_black (:enabled t)
                                    :pyls_isort (:enabled t)))))

  (aero-leader-def
    "la" 'eglot-code-actions
    "lf" '(:ignore t :wk "find")
    "lfr" 'xref-find-references
    "lfd" 'eglot-find-declaration
    "lfi" 'eglot-find-implementation
    "lft" 'eglot-find-typeDefinition
    "lr" '(:ignore t :wk "refactor")
    "lrr" 'eglot-rename
    "lrf" 'eglot-format
    "lro" 'eglot-code-action-organize-imports))

;; Optimizations to Eglot, using emacs-lsp-booster under the hood. emacs-lsp-booster must have been
;; installed already (its a Rust binary), which can be done with `make install-deps' or the more
;; specific `make lsp-booster'
(package! eglot-booster "jdtsmith/eglot-booster"
  :after eglot
  :config (eglot-booster-mode))

;; Make eglot send more info to eldoc, including parameter and function documentation
(package! eglot-signature-eldoc-talkative
  (:host codeberg :repo "mekeor/eglot-signature-eldoc-talkative" :branch "default")
  :after (eglot)
  :config (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative))

;; puts eldoc in a child frame instead of the echo area
(package! eldoc-box "casouri/eldoc-box"
  :after general

  :preface
  (defun aero/eldoc-set-documentation-strategy ()
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose))

  (defun aero/eldoc-box-help-at-point ()
    "Display full eldoc at point on command"
    (interactive)
    (let ((eldoc-echo-area-use-multiline-p t))
      (call-interactively #'eldoc-box-help-at-point)))

  ;; Fix documentation strategy to show all of the available eldoc information when we want it. This
  ;; way Flymake errors don't just get clobbered by docstrings.
  :hook (eglot-managed-mode . aero/eldoc-set-documentation-strategy)

  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-box-only-multi-line nil) ; leave single-line docs in minibuffer
  (eldoc-echo-area-use-multiline-p nil) ; normally use one line unless requested
  (eldoc-box-max-pixel-height 99999)
  (eldoc-box-max-pixel-width 99999)

  :init
  (aero-leader-def "i" 'aero/eldoc-box-help-at-point)

  :config
  ;; attempt to prettify typescript errors
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))


;;; System-specifics

;; Mac needs some extra hand-holding to connect the kill-ring to the system
;; clipboard.
(when (system-is-mac)
  (declare-function aero/pbcopier-select-text "aero-pbcopier.el")
  (declare-function aero/pbcopier-selection-value "aero-pbcopier.el")
  (setq interprogram-cut-function #'aero/pbcopier-select-text)
  (setq interprogram-paste-function #'aero/pbcopier-selection-value)

  (setq-default ns-use-native-fullscreen nil)
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF")))

;; Linux just needs the functionality enabled
(when (system-is-linux)
  (setq select-enable-clipboard t
        interprogram-paste-function #'gui-selection-value
        x-gtk-use-system-tooltips t
        dired-listing-switches "-lFaGh1v --group-directories-first"))


;;; File navigation

;; We use the most up-to-date tramp instead of the built-in since it gave us
;; trouble in the past
(package! tramp (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :defer t
  :functions tramp-cleanup-all-connection
  :custom
  (tramp-auto-save-directory
   (expand-file-name "tramp/autosave" aero-cache-dir))
  (tramp-persistency-file-name
   (expand-file-name "tramp/persistency" aero-cache-dir))
  (tramp-use-ssh-controlmaster-options nil)  ; use system settings instead
  (tramp-default-method "rsync")
  (tramp-terminal-type "tramp"))

(package! dired :builtin
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-async-mode))
  :bind (:map dired-mode-map
         ("M-n" . #'dired-next-dirline)
         ("M-p" . #'dired-prev-dirline)
         ("TAB" . #'dired-next-subdir)))


;;; Editor setup

(package! editorconfig "editorconfig/editorconfig-emacs"
  :defer 1
  :functions (editorconfig-mode)
  :config (editorconfig-mode +1))

;; detects when the buffer matches what's on disk and marks it unmodified. If, for example, you
;; visit a file, change something, then undo the change, this package ensures the buffer doesn't
;; think its still modified.
(package! unmodified-buffer "arthurcgusmao/unmodified-buffer"
  :defer 1
  :hook ((prog-mode text-mode) . unmodified-buffer-mode))

;; Use `so-long-revert' in a buffer to get back to what it would otherwise have loaded as.
(package! so-long :builtin
  :config (global-so-long-mode +1))

(package! savehist :builtin
  :init (savehist-mode)
  :custom (savehist-file (expand-file-name "history" aero-cache-dir)))


(provide 'aero-prelude)
;;; aero-prelude.el ends here
