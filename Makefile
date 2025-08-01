# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs
EMACS_BUILD_DIR ?= ~/lib/emacs/

macos: upgrade-emacs-macos install-macos
	@echo "Installed, run make init to install dependencies, and make clear-straight-build to rebuild all packages on next start"

linux: build-emacs-linux install-linux
	@echo "Installed, run make init to install dependencies, and make clear-straight-build to rebuild all packages on next start"

# required, emacs-plus handles the actual Emacs dependencies
macos-reqs:
	brew update
	brew install coreutils git-delta tree-sitter
	brew tap d12frosted/emacs-plus

build-emacs-cask-stable: macos-reqs
	brew install --cask emacs

build-emacs-cask: macos-reqs
	brew install --cask emacs-nightly

build-emacs-macos: macos-reqs
	brew install emacs-plus@30 --with-savchenkovaleriy-big-sur-3d-icon --with-xwidgets --with-dbus
	ln -sf /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications

# for when libgccjit breaks every few months
# NOTE: dbus isn't working on M1 yet.
build-emacs-macos-minimal: macos-reqs
	brew install emacs-plus@30 --with-savchenkovaleriy-big-sur-3d-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications

build-emacs-macos-stable: macos-reqs
	brew install emacs-plus@29 --with-savchenkovaleriy-big-sur-3d-icon --with-native-comp --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

build-emacs-macos-stable-minimal: macos-reqs
	brew install emacs-plus@29 --with-savchenkovaleriy-big-sur-3d-icon --with-xwidgets
	ln -sf /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications

remove-emacs-macos:
	brew uninstall emacs-plus@30 || true

remove-emacs-macos-stable:
	brew uninstall emacs-plus@29 || true

upgrade-emacs-macos: remove-emacs-macos build-emacs-macos

install-macos:
	osacompile -o bin/Emacs.app bin/aero-emacs.osx.applescript
	cp etc/logo/Emacs.icns bin/Emacs.app/Contents/Resources/applet.icns
	[ -s /Applications/Emacs.app ] && rm -rf /Applications/Emacs.app || true
	mv bin/Emacs.app /Applications/

clean-aero-macos:
	rm -rf /Applications/Emacs.app

build-emacs-linux: 
	./bin/build/linux.zsh

install-linux:
	mkdir -p ~/.local/share/applications/
	cp ./bin/aero-emacs.desktop ~/.local/share/applications/

.PHONY: nongnu-elpa
nongnu-elpa:
	# nongnu-elpa is corrupted somehow, this fixes it by cloning without fsck whether or not it's already there
	rm -rf ~/.config/emacs/straight/repos/nongnu-elpa/
	mkdir -p ~/.config/emacs/straight/repos/
	git clone https://git.savannah.gnu.org/git/emacs/nongnu.git ~/.config/emacs/straight/repos/nongnu-elpa --config transfer.fsckobjects=false --config receive.fsckobjects=false --config fetch.fsckobjects=false

submodule:
	git submodule init
	git submodule update

init: nongnu-elpa install-deps submodule

clear-straight:
	rm -rf ./straight/

clear-straight-build:
	rm -rf ./straight/build

# Clear out packages and re-init
hard-init: clear-straight init

# Continues even on failures. This lets us only install what the system can install, but can
# swallow up errors
install-deps: lsp-booster graphviz install-npm
	brew install node ripgrep
	gem install bundler prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs && npm i -g prettier @prettier/plugin-ruby || true

	pip install python-lsp-server pyls-mypy pyls-black pyls-isort mypy ruff black "ptvsd>=4.2" || true

	rustup component add rls rust-analysis rust-src || true
	brew install clojure-lsp/brew/clojure-lsp-native || true
	brew install aspell
	brew install cmake

install-npm:
	npm i -g bash-language-server @types/node || true
	npm i -g @angular/language-service@next typescript @angular/language-server typescript-language-server eslint @elm-tooling/elm-language-server @astrojs/language-server || true
	npm i -g emmet-ls vscode-json-languageserver || true

# Required for org-roam graphing
graphviz:
	brew install graphviz

lsp-booster:
	if [ -d ~/.config/emacs/tmp/emacs-lsp-booster ]; then \
		cd ~/.config/emacs/tmp/emacs-lsp-booster && git pull; \
	else git clone git@github.com:blahgeek/emacs-lsp-booster.git ~/.config/emacs/tmp/emacs-lsp-booster; \
	fi
	cd ~/.config/emacs/tmp/emacs-lsp-booster && cargo build --release
	mkdir -p ~/.local/bin
	ln -sf ~/.config/emacs/tmp/emacs-lsp-booster/target/release/emacs-lsp-booster ~/.local/bin/emacs-lsp-booster

.PHONY: export
export: index.html

# Export config to HTML. Uses htmlize to fontify the code blocks
index.html: config.org
	emacs --batch \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(unless (package-installed-p 'htmlize) (package-install 'htmlize))" \
	  config.org \
		--eval "(add-to-list 'custom-theme-load-path (concat default-directory \"lib/aero-theme\"))" \
	  --eval "(load-theme 'aero t)" \
	  --eval "(require 'org)" \
	  --eval "(require 'htmlize)" \
	  --eval "(setq org-html-wrap-src-lines t)" \
	  --eval "(org-html-export-to-html)"
