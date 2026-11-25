# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs
EMACS_BUILD_DIR ?= ~/lib/emacs/

macos: sync upgrade-emacs-macos init
	@echo "Installed"

linux: sync build-emacs-linux install-linux init
	@echo "Installed"

# required, emacs-plus handles the actual Emacs dependencies
macos-reqs:
	brew update
	brew install coreutils git-delta tree-sitter
	brew tap d12frosted/emacs-plus
	brew trust --tap d12frosted/emacs-plus

build-emacs-cask-stable: macos-reqs
	brew install --cask emacs

build-emacs-cask: macos-reqs
	brew install --cask emacs-nightly

# Install Emacs.app to /Applications (use cp instead of ln because /Applications has sunlnk flag set by SIP/MDM on my work machine)
install-emacs-macos:
	rm -rf /Applications/Emacs.app || true
	cp -r /opt/homebrew/opt/emacs-plus@31/Emacs.app /Applications/

build-emacs-macos: macos-reqs
	brew install emacs-plus@31 --with-xwidgets --with-dbus
	$(MAKE) install-emacs-macos

# for when libgccjit breaks every few months
build-emacs-macos-minimal: macos-reqs
	brew install emacs-plus@31
	$(MAKE) install-emacs-macos

remove-emacs-macos:
	brew uninstall emacs-plus@31 || true
	brew uninstall emacs-plus@30 || true

upgrade-emacs-macos: remove-emacs-macos build-emacs-macos

clean-emacs-macos:
	rm -rf /Applications/Emacs.app

build-emacs-linux:
	./bin/build/linux.zsh

install-linux:
	mkdir -p ~/.local/share/applications/
	cp ./bin/aero-emacs.desktop ~/.local/share/applications/

submodule:
	git submodule update --init --recursive

# pull the superproject and bring all submodules into sync, including newly added drones
sync:
	git pull --recurse-submodules
	git submodule update --init --recursive

.PHONY: borg
borg: sync borg-config borg-autoloads

# repo-local git config for a comfortable submodule/drone workflow
borg-config:
	git config submodule.recurse true
	git config diff.submodule log
	git config status.submoduleSummary true
	git config borg.drones-directory lib/drones

# (re)generate autoloads for all Borg drones. This step is dependency-free and never compiles
borg-autoloads:
	$(EMACS) --batch -L lib/borg -l borg \
		--eval "(setq borg-drones-directory (expand-file-name \"lib/drones\"))" \
		--eval "(borg-do-drones (d) (borg-update-autoloads d))"

# byte-compile every drone. All drones are on the load path first, so each compiles against its
# dependencies whatever the build order. Emacs then makes the .elc files native on first load
.PHONY: borg-build
borg-build:
	$(EMACS) --batch -Q -L lib/borg -l borg \
		--eval "(setq borg-drones-directory (expand-file-name \"lib/drones\"))" \
		--eval "(borg-initialize)" \
		--eval "(borg-do-drones (d) (borg-build d))"

# rebuild the org-roam cache db when org-roam's schema version changed (see the script header). Runs
# batch -Q with native compilation off, so it never loads config.el or triggers async compiles
.PHONY: org-roam-db-upgrade
org-roam-db-upgrade:
	$(EMACS) --batch -Q -l bin/org-roam-db-upgrade.el

init: install-deps submodule borg borg-build org-roam-db-upgrade

# Continues even on failures. This lets us only install what the system can install, but can
# swallow up errors
install-aspell:
	brew install aspell

install-deps: graphviz install-npm install-aspell
	brew install node ripgrep
	if [ "$$(uname)" = "Darwin" ]; then brew install pngpaste; fi
	if [ "$$(uname)" = "Linux" ]; then sudo apt install -y wl-clipboard; fi
	gem install bundler prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs && npm i -g prettier @prettier/plugin-ruby || true

	pip install python-lsp-server pyls-mypy mypy ruff black "ptvsd>=4.2" || true

	brew install cmake

install-npm:
	npm i -g bash-language-server @types/node || true
	npm i -g @angular/language-service@next typescript @angular/language-server typescript-language-server eslint @elm-tooling/elm-language-server @astrojs/language-server || true
	npm i -g emmet-ls vscode-json-languageserver || true

# Required for org-roam graphing
graphviz:
	brew install graphviz
