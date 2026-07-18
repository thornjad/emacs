# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture

This is a single-file Emacs configuration:

- **Main configuration**: `config.el` - Plain Emacs Lisp, organized into sections with `;;;`-style outline headings (navigate with `outline-minor-mode`/imenu, not org headlines)
- **Bootstrap**: `init.el` - Minimal bootstrap that loads `config.el` directly
- **Package management**: Uses `straight.el` for package management (installed automatically on first run)
- **Theme**: Custom `aero-theme` located in `lib/aero-theme/`
- **Modular structure**: Configuration is organized in sections within `config.el`

`config.el` was converted from a literate org-mode config (`config.org`, now removed) in July 2026 to cut memory overhead and the org-babel tangle step from the load path. All settings, functions, and package configurations live directly in `config.el`, with prose explanation as comments above each section.

## Common Development Commands

### Initial Setup
- `make init` - Full initialization: installs dependencies, sets up submodules, and fixes nongnu-elpa if corrupted
- `make nongnu-elpa` - Fix corrupted nongnu-elpa repository (common issue)
- `make submodule` - Initialize and update git submodules

### Package Management
- `make clear-straight` - Remove all straight.el packages (nuclear option)
- `make clear-straight-build` - Clear package build cache (gentler reset)
- `make hard-init` - Clear packages and re-initialize everything

### Development Dependencies
- `make install-deps` - Install LSP servers and development tools (includes Python, Rust components)
- `make install-npm` - Install Node.js-based language servers (TypeScript, Angular, Elm, etc.)
- `make lsp-booster` - Build and install emacs-lsp-booster for faster LSP performance
- `make graphviz` - Install graphviz (required for org-roam graph visualization)

### Emacs Installation (if needed)
- `make macos` - Full MacOS installation with Emacs build
- `make linux` - Full Linux installation with Emacs build
- Various Emacs build options: `build-emacs-macos`, `build-emacs-cask`, etc.

## Key Configuration Patterns

### Evil Mode Setup
The configuration heavily uses Evil mode for Vim-like editing with extensive custom bindings centered around `SPC` as the leader key. Common patterns:
- `SPC SPC` - Execute command (M-x equivalent)
- `SPC f` - File operations (save, open, etc.)
- `SPC b` - Buffer operations
- `SPC p` - Project operations
- `SPC g` - Git operations via Magit

### Package Configuration Structure
Most packages follow this pattern in `config.el`:
```elisp
(package! package-name :auto ; or repo name
  :config
  ;; configuration here
  )
```

**Package Source Preference**: Use `:auto` for packages available on MELPA rather than specifying GitHub repo names, as this provides better stability and version management through the package archive system.

### Custom Functions and Macros
Many helper functions are defined inline within the configuration, typically prefixed with `aero/`. Key examples:
- `aero/keyboard-quit-context` - Enhanced quit behavior
- `aero/` prefixed utilities throughout the config

### Local Configuration Support
The system supports `init.local.el` for machine-specific configuration that shouldn't be committed. This file is loaded last and can contain secrets, work-specific functions, or environment variables.

### Garbage Collection Optimization
The configuration implements controversial but potentially beneficial GC tuning:
- High thresholds during startup and minibuffer use
- Lower thresholds during normal operation
- Configured via `aero/gc-cons` variable

## Important Files to Understand

- `config.el` - The main configuration file (plain Emacs Lisp, `;;;`-style outline headings)
  - Early sections: core setup, directory constants, package management bootstrap
  - Contains all package configurations, custom functions, and keybindings
  - Loaded directly by `init.el`, no tangle/build step
- `init.el` - Bootstrap file that loads `config.el` with GC optimization
- `early-init.el` - Early initialization (GUI tweaks, UTF-8 setup, package system disable)
- `lib/aero-theme/` - Custom theme implementation (aero-light and aero-dark)
- `Makefile` - All build, installation, and maintenance commands
- `init.local.el` - Optional machine-specific configuration (not tracked in git)

## Configuration Philosophy

This is a highly personal configuration optimized for:
- **Speed**: Uses straight.el for package management, native compilation, GC tuning
- **Modal editing**: Evil mode with extensive customization, SPC-based leader key system
- **Development workflows**: Comprehensive LSP setup with eglot, multiple language support
- **Aesthetic consistency**: Custom aero-theme and modeline implementation
- **Single-file configuration**: Everything documented and organized in config.el, with outline headings for navigation
- **Thornlog integration**: Custom daily logging and note-taking system using org-roam

The configuration is not designed as a distribution but as a personal system that evolves constantly.

## Development Workflow Integration

### Supported Languages and Tools
- **JavaScript/TypeScript**: Full LSP support with typescript-language-server
- **Python**: python-lsp-server with mypy, black, ruff integration
- **Rust**: Built-in rust-analyzer support via rustup
- **Clojure**: CIDER with clojure-lsp integration
- **Elm**: elm-language-server support
- **Angular**: Specialized @angular/language-server support
- **Go, Java, C/C++**: Various LSP integrations available

### Key Development Features
- **Eglot**: Built-in LSP client with booster optimization
- **Magit**: Full Git integration with evil-mode bindings
- **Project management**: Built-in project.el with custom enhancements
- **Company mode**: Auto-completion with company-box UI
- **Apheleia**: Automatic code formatting on save
- **Tree-sitter**: Syntax highlighting for supported languages

## Code Organization and Development Patterns

### Function Naming Convention
All custom functions follow the `aero/` prefix convention. Common patterns:
- **Interactive functions**: Use `(interactive)` for user-callable commands
- **Shell commands**: Use `shell-command` for synchronous, `async-shell-command` for non-blocking
- **Directory context**: Use `(let ((default-directory path)) ...)` to run commands in specific directories

### Configuration Structure in `config.el`
- **"Directory constants"** section: Path definitions and environment setup
- **"Keybindings" → "General"** section: Key bindings defined using general.el with Evil mode leader keys
- **"Org mode and org agenda"** section: Personal workflow functions and org-mode customizations
- **"Org-roam" → "Thornlog"** subsection: Personal logging and task management functions

### Common Code Execution Patterns
```elisp
;; Synchronous shell command
(shell-command "command")

;; Async with output buffer
(async-shell-command "command" "*Buffer Name*")

;; Directory-specific execution
(let ((default-directory target-directory))
  (shell-command "command"))

;; Interactive function template
(defun aero/function-name ()
  "Description of what this function does."
  (interactive)
  ;; function body
  )
```

### Key Binding Organization
- Uses `general.el` for key binding management
- Leader key is `SPC` (space)
- Organized hierarchically: `SPC f` (files), `SPC b` (buffers), `SPC p` (projects)
- Custom functions typically bound under logical prefixes

### Search Tips for Development
When looking for similar functionality:
1. Search for `defun aero/` to find custom function definitions
2. Search for specific command patterns like `shell-command` or `async-shell-command`
3. Key bindings are defined in the **"Keybindings" → "General"** section
4. Constants and paths are defined in the **"Directory constants"** section
5. Package configurations follow the `(package! name ...)` pattern throughout the file

## Configuration Testing and Debugging

### Common Issues and Solutions
- **Startup errors**: Use `make clear-straight-build` to rebuild packages, or `make hard-init` for complete reset
- **Package conflicts**: Check `*Messages*` buffer for conflicts; may need `make clear-straight` (nuclear option)
- **LSP issues**: Verify language servers are installed via `make install-deps` or `make install-npm`
- **Theme issues**: Custom themes are in `lib/aero-theme/` and loaded via directory constant

### Configuration Reloading Guidelines
- **NEVER reload the entire configuration** without explicit user instruction (`load`-ing all of `config.el` re-runs every `package!` block and is slow and disruptive)
- **For config.el changes**: Attempt to reload just the changed form via Emacs's own tools:
  ```elisp
  (with-current-buffer (find-file-noselect (expand-file-name "config.el" user-emacs-directory))
    (goto-char (point-min))
    (when (re-search-forward "defun aero/some-function" nil t)
      (eval-defun nil)))
  ```
  For a non-`defun` top-level form (e.g. a `package!` block or `setq`), use `eval-last-sexp` after point is past the closing paren, or select the region and use `eval-region`.
- **If reload fails**: Stop and ask the user to evaluate the specific section, providing:
  - Exact line number or section name to navigate to
  - Clear instructions like "Go to line 3940 (Claude Code MCP Interface section) and evaluate the form with C-M-x"
  - Which specific function or feature needs to be available

### Validation Commands
- NEVER restart Emacs without the user's consent
- Check for byte-compilation warnings in `*Compile-Log*` buffer
