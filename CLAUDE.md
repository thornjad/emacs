# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture

This is a comprehensive Emacs configuration using a literate programming approach:

- **Main configuration**: `config.org` - The primary configuration file written in org-mode with embedded Emacs Lisp
- **Bootstrap**: `init.el` - Minimal bootstrap that loads the org configuration using `org-babel-load-file`
- **Package management**: Uses `straight.el` for package management (installed automatically on first run)
- **Theme**: Custom `aero-theme` located in `lib/aero-theme/`
- **Modular structure**: Configuration is organized in sections within `config.org`

The configuration follows a literate programming pattern where all settings, functions, and package configurations are documented inline within the org file, then tangled into `config.el` for execution.

## Common Development Commands

### Initial Setup
- `make init` - Full initialization: installs dependencies, sets up submodules, and fixes nongnu-elpa if corrupted
- `make nongnu-elpa` - Fix corrupted nongnu-elpa repository (common issue)
- `make submodule` - Initialize and update git submodules

### Package Management
- `make clear-straight` - Remove all straight.el packages (nuclear option)
- `make clear-straight-build` - Clear package build cache (gentler reset)
- `make hard-init` - Clear packages and re-initialize everything

### Documentation Export
- `make export` - Export `config.org` to HTML with syntax highlighting
- The exported HTML is viewable at https://emacs.jmthornton.net

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
Most packages follow this pattern in `config.org`:
```elisp
(package! package-name :auto ; or repo name
  :config
  ;; configuration here
  )
```

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

- `config.org` - The main literate configuration file (org-mode with embedded Emacs Lisp)
  - Lines 1-200: Core setup, directory constants, package management bootstrap
  - Contains all package configurations, custom functions, and keybindings
  - Tangled to `config.el` on startup via `org-babel-load-file`
- `init.el` - Bootstrap file that loads the org configuration with GC optimization
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
- **Literate configuration**: Everything documented and organized in config.org
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
