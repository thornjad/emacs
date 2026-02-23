# -*- indent-tabs-mode: t; -*-

# override to use something like, say, a local version of remacs
EMACS ?= emacs
EMACS_BUILD_DIR ?= ~/lib/emacs/

macos: upgrade-emacs-macos precompile
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

# Install Emacs.app to /Applications (use cp instead of ln because /Applications has sunlnk flag set by SIP/MDM on my work machine)
install-emacs-macos:
	rm -rf /Applications/Emacs.app || true
	cp -r /opt/homebrew/opt/emacs-plus@31/Emacs.app /Applications/

install-emacs-macos-stable:
	rm -rf /Applications/Emacs.app || true
	cp -r /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications/

build-emacs-macos: macos-reqs
	brew install emacs-plus@31 --with-xwidgets --with-dbus
	$(MAKE) install-emacs-macos

# for when libgccjit breaks every few months
build-emacs-macos-minimal: macos-reqs
	brew install emacs-plus@31
	$(MAKE) install-emacs-macos

build-emacs-macos-stable: macos-reqs
	brew install emacs-plus@30 --with-native-comp --with-xwidgets
	$(MAKE) install-emacs-macos-stable

build-emacs-macos-stable-minimal: macos-reqs
	brew install emacs-plus@30
	$(MAKE) install-emacs-macos-stable

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
install-hunspell:
	brew install hunspell
	mkdir -p ~/Library/Spelling
	curl -fsSL "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic" -o ~/Library/Spelling/en_US.dic
	curl -fsSL "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff" -o ~/Library/Spelling/en_US.aff
	@if head -1 $(HOME)/Documents/thornlog/ispell/personal_dictionary.aws | grep -q "^personal_ws"; then \
		tail -n +2 $(HOME)/Documents/thornlog/ispell/personal_dictionary.aws > /tmp/dict_tmp && \
		mv /tmp/dict_tmp $(HOME)/Documents/thornlog/ispell/personal_dictionary.aws; \
		echo "Converted personal dictionary from aspell to hunspell format"; \
	else \
		echo "Personal dictionary already in hunspell format"; \
	fi

install-deps: lsp-booster graphviz install-npm install-hunspell
	brew install node ripgrep
	gem install bundler prettier_print syntax_tree syntax_tree-haml syntax_tree-rbs && npm i -g prettier @prettier/plugin-ruby || true

	pip install python-lsp-server pyls-mypy pyls-black pyls-isort mypy ruff black "ptvsd>=4.2" jedhy || true

	rustup component add rls rust-analysis rust-src || true
	brew install clojure-lsp/brew/clojure-lsp-native || true
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

# Pre-compile all packages for native compilation to avoid slow first launch
# Works on fresh installs: sets up recipe repos, tangles config, bootstraps straight.el, installs packages, then compiles
.PHONY: precompile
precompile: precompile-setup-repos precompile-tangle
	@echo "Loading configuration and installing packages (this may take several minutes)..."
	@/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
		--eval "(setq native-comp-async-report-warnings-errors 'silent)" \
		--eval "(setq native-comp-async-jobs-number 4)" \
		-l ~/.config/emacs/init.el \
		--eval "(message \"Configuration loaded, starting native compilation...\")" \
		--eval "(native-compile-async \"~/.config/emacs/straight/build/\" 'recursively)" \
		--eval "(let ((start-time (current-time)) (last-count 0)) \
		         (while (> (length comp-files-queue) 0) \
		           (let ((current-count (length comp-files-queue))) \
		             (when (/= current-count last-count) \
		               (message \"Compiling... %d files remaining\" current-count) \
		               (setq last-count current-count))) \
		           (sleep-for 2)) \
		         (message \"Native compilation complete in %.1f seconds\" \
		                  (float-time (time-subtract (current-time) start-time))))"
	@echo "Pre-compilation finished."

# Tangle config.org to config.el so it doesn't need to be tangled on first interactive startup
.PHONY: precompile-tangle
precompile-tangle:
	@echo "Tangling config.org to config.el..."
	@/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
		-l org \
		--eval "(org-babel-tangle-file \"~/.config/emacs/config.org\")"

# Clone essential recipe repositories for straight.el if they don't exist (needed for batch bootstrap)
.PHONY: precompile-setup-repos
precompile-setup-repos:
	@mkdir -p ~/.config/emacs/straight/repos
	@if [ ! -d ~/.config/emacs/straight/repos/melpa ]; then \
		echo "Cloning melpa recipes..."; \
		git clone --depth 1 https://github.com/melpa/melpa.git ~/.config/emacs/straight/repos/melpa; \
	fi
	@if [ ! -d ~/.config/emacs/straight/repos/gnu-elpa-mirror ]; then \
		echo "Cloning gnu-elpa-mirror recipes..."; \
		git clone --depth 1 https://github.com/emacs-straight/gnu-elpa-mirror.git ~/.config/emacs/straight/repos/gnu-elpa-mirror; \
	fi
	@if [ ! -d ~/.config/emacs/straight/repos/nongnu-elpa ]; then \
		echo "Cloning nongnu-elpa recipes..."; \
		git clone https://git.savannah.gnu.org/git/emacs/nongnu.git ~/.config/emacs/straight/repos/nongnu-elpa \
			--config transfer.fsckobjects=false --config receive.fsckobjects=false --config fetch.fsckobjects=false; \
	fi
	@if [ ! -d ~/.config/emacs/straight/repos/emacsmirror-mirror ]; then \
		echo "Cloning emacsmirror-mirror recipes..."; \
		git clone --depth 1 https://github.com/emacs-straight/emacsmirror-mirror.git ~/.config/emacs/straight/repos/emacsmirror-mirror; \
	fi
	@if [ ! -d ~/.config/emacs/straight/repos/straight.el ]; then \
		echo "Cloning straight.el..."; \
		git clone --depth 1 https://github.com/radian-software/straight.el.git ~/.config/emacs/straight/repos/straight.el; \
	fi

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
