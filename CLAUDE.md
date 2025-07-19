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
- `make install-deps` - Install LSP servers and development tools
- `make install-npm` - Install Node.js-based language servers
- `make lsp-booster` - Build and install emacs-lsp-booster for faster LSP performance

### Emacs Installation (if needed)
- `make macos` - Full MacOS installation with Emacs build
- `make linux` - Full Linux installation with Emacs build
- Various Emacs build options: `build-emacs-macos`, `build-emacs-cask`, etc.

## Key Configuration Patterns

### Evil Mode Setup
The configuration heavily uses Evil mode for Vim-like editing with extensive custom bindings centered around `SPC` as the leader key.

### Package Configuration Structure
Most packages follow this pattern in `config.org`:
```elisp
(package! package-name :auto ; or repo name
  :config
  ;; configuration here
  )
```

### Custom Functions and Macros
Many helper functions are defined inline within the configuration, typically prefixed with `aero/`.

### Local Configuration Support
The system supports `init.local.el` for machine-specific configuration that shouldn't be committed.

## Important Files to Understand

- `config.org:1-200` - Contains the foundational setup and package management
- `init.el` - Bootstrap and garbage collection optimization
- `early-init.el` - Early initialization (GUI tweaks, package setup)
- `lib/aero-theme/` - Custom theme implementation
- `Makefile` - All build and maintenance commands

## Configuration Philosophy

This is a highly personal configuration optimized for:
- Speed (straight.el, native compilation)
- Modal editing (Evil mode with extensive customization)
- Development workflows (comprehensive LSP setup, multiple language support)
- Aesthetic consistency (custom theme and modeline)
- Literate configuration (everything documented in config.org)

The configuration is not designed as a distribution but as a personal system that evolves constantly.
