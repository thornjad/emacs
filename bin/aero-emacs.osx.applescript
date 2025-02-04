-- To install, run make install-macos
-- NOTE: Requires running in Zsh

tell application "Terminal"
	do script "cd ~ && source ~/.zshrc && LSP_USE_PLISTS=true emacs -mm &| exit"
	activate
end tell
