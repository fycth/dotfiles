# Dotfiles Makefile - Cross-platform (macOS/Linux)
# Re-entrant: safe to run multiple times

SHELL := /bin/bash
DOTFILES := $(shell pwd)
HOME_DIR := $(HOME)
OS := $(shell uname -s)

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[0;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

# Git repositories to clone
OH_MY_ZSH_REPO := https://github.com/ohmyzsh/ohmyzsh.git
OH_MY_ZSH_DIR := $(HOME_DIR)/.oh-my-zsh

P10K_REPO := https://github.com/romkatv/powerlevel10k.git
P10K_DIR := $(DOTFILES)/custom-zsh/themes/powerlevel10k

ZSH_COMPLETIONS_REPO := https://github.com/zsh-users/zsh-completions.git
ZSH_COMPLETIONS_DIR := $(DOTFILES)/custom-zsh/plugins/zsh-completions

ZSH_SYNTAX_REPO := https://github.com/zsh-users/zsh-syntax-highlighting.git
ZSH_SYNTAX_DIR := $(DOTFILES)/custom-zsh/plugins/zsh-syntax-highlighting

# Files to symlink (source -> target)
SYMLINKS := \
	.zshrc \
	.zshenv.sh \
	.zlogin.sh \
	.gitconfig \
	.gitignore_global \
	.gitmessage \
	.git_template \
	.mvn

# Directories to symlink
DIR_SYMLINKS := \
	.emacs.d \
	.config/nvim

.PHONY: all install check clean update help deps symlinks

all: install

help:
	@echo "Dotfiles Management"
	@echo ""
	@echo "Usage:"
	@echo "  make install  - Install everything (deps + symlinks)"
	@echo "  make check    - Check what's installed vs missing"
	@echo "  make deps     - Clone git dependencies only"
	@echo "  make symlinks - Create symlinks only"
	@echo "  make update   - Update git-cloned dependencies"
	@echo "  make clean    - Remove symlinks (keeps cloned repos)"
	@echo ""

install: deps symlinks
	@echo ""
	@echo -e "$(GREEN)✓ Installation complete!$(NC)"
	@echo ""
	@$(MAKE) --no-print-directory check-tools

# Clone git dependencies
deps: $(OH_MY_ZSH_DIR) $(P10K_DIR) $(ZSH_COMPLETIONS_DIR) $(ZSH_SYNTAX_DIR)

$(OH_MY_ZSH_DIR):
	@echo -e "$(BLUE)Cloning oh-my-zsh...$(NC)"
	@git clone --depth=1 $(OH_MY_ZSH_REPO) $@

$(P10K_DIR):
	@echo -e "$(BLUE)Cloning powerlevel10k...$(NC)"
	@mkdir -p $(dir $@)
	@git clone --depth=1 $(P10K_REPO) $@

$(ZSH_COMPLETIONS_DIR):
	@echo -e "$(BLUE)Cloning zsh-completions...$(NC)"
	@mkdir -p $(dir $@)
	@git clone --depth=1 $(ZSH_COMPLETIONS_REPO) $@

$(ZSH_SYNTAX_DIR):
	@echo -e "$(BLUE)Cloning zsh-syntax-highlighting...$(NC)"
	@mkdir -p $(dir $@)
	@git clone --depth=1 $(ZSH_SYNTAX_REPO) $@

# Create symlinks
symlinks:
	@echo -e "$(BLUE)Creating symlinks...$(NC)"
	@# Regular file symlinks
	@for file in $(SYMLINKS); do \
		src="$(DOTFILES)/$$file"; \
		dst="$(HOME_DIR)/$$file"; \
		if [ -e "$$src" ]; then \
			if [ -L "$$dst" ]; then \
				echo -e "  $(YELLOW)↷ $$file (already linked)$(NC)"; \
			elif [ -e "$$dst" ]; then \
				echo -e "  $(RED)✗ $$file (exists, skipping - backup manually)$(NC)"; \
			else \
				ln -s "$$src" "$$dst"; \
				echo -e "  $(GREEN)✓ $$file$(NC)"; \
			fi \
		fi \
	done
	@# Directory symlinks
	@for dir in $(DIR_SYMLINKS); do \
		src="$(DOTFILES)/$$dir"; \
		dst="$(HOME_DIR)/$$dir"; \
		parent_dir=$$(dirname "$$dst"); \
		if [ -e "$$src" ]; then \
			mkdir -p "$$parent_dir"; \
			if [ -L "$$dst" ]; then \
				echo -e "  $(YELLOW)↷ $$dir (already linked)$(NC)"; \
			elif [ -e "$$dst" ]; then \
				echo -e "  $(RED)✗ $$dir (exists, skipping - backup manually)$(NC)"; \
			else \
				ln -s "$$src" "$$dst"; \
				echo -e "  $(GREEN)✓ $$dir$(NC)"; \
			fi \
		fi \
	done
	@# Dotfiles directory symlink (for ZSH_CUSTOM)
	@if [ "$(DOTFILES)" != "$(HOME_DIR)/dotfiles" ]; then \
		if [ -L "$(HOME_DIR)/dotfiles" ]; then \
			echo -e "  $(YELLOW)↷ ~/dotfiles (already linked)$(NC)"; \
		elif [ -e "$(HOME_DIR)/dotfiles" ]; then \
			echo -e "  $(RED)✗ ~/dotfiles (exists, skipping)$(NC)"; \
		else \
			ln -s "$(DOTFILES)" "$(HOME_DIR)/dotfiles"; \
			echo -e "  $(GREEN)✓ ~/dotfiles -> $(DOTFILES)$(NC)"; \
		fi \
	fi

# Update cloned repositories
update:
	@echo -e "$(BLUE)Updating dependencies...$(NC)"
	@if [ -d "$(OH_MY_ZSH_DIR)" ]; then \
		echo "  Updating oh-my-zsh..."; \
		cd "$(OH_MY_ZSH_DIR)" && git pull --ff-only; \
	fi
	@if [ -d "$(P10K_DIR)" ]; then \
		echo "  Updating powerlevel10k..."; \
		cd "$(P10K_DIR)" && git pull --ff-only; \
	fi
	@if [ -d "$(ZSH_COMPLETIONS_DIR)" ]; then \
		echo "  Updating zsh-completions..."; \
		cd "$(ZSH_COMPLETIONS_DIR)" && git pull --ff-only; \
	fi
	@if [ -d "$(ZSH_SYNTAX_DIR)" ]; then \
		echo "  Updating zsh-syntax-highlighting..."; \
		cd "$(ZSH_SYNTAX_DIR)" && git pull --ff-only; \
	fi
	@echo -e "$(GREEN)✓ Update complete$(NC)"

# Remove symlinks
clean:
	@echo -e "$(BLUE)Removing symlinks...$(NC)"
	@for file in $(SYMLINKS); do \
		dst="$(HOME_DIR)/$$file"; \
		if [ -L "$$dst" ]; then \
			rm "$$dst"; \
			echo -e "  $(GREEN)✓ Removed $$file$(NC)"; \
		fi \
	done
	@for dir in $(DIR_SYMLINKS); do \
		dst="$(HOME_DIR)/$$dir"; \
		if [ -L "$$dst" ]; then \
			rm "$$dst"; \
			echo -e "  $(GREEN)✓ Removed $$dir$(NC)"; \
		fi \
	done
	@if [ -L "$(HOME_DIR)/dotfiles" ] && [ "$$(readlink $(HOME_DIR)/dotfiles)" = "$(DOTFILES)" ]; then \
		rm "$(HOME_DIR)/dotfiles"; \
		echo -e "  $(GREEN)✓ Removed ~/dotfiles$(NC)"; \
	fi
	@echo -e "$(GREEN)✓ Clean complete$(NC)"

# Check installation status
check:
	@echo ""
	@echo -e "$(BLUE)=== Dotfiles Status ===$(NC)"
	@echo ""
	@echo -e "$(BLUE)Git Dependencies:$(NC)"
	@if [ -d "$(OH_MY_ZSH_DIR)" ]; then \
		echo -e "  $(GREEN)✓$(NC) oh-my-zsh"; \
	else \
		echo -e "  $(RED)✗$(NC) oh-my-zsh (run: make deps)"; \
	fi
	@if [ -d "$(P10K_DIR)" ]; then \
		echo -e "  $(GREEN)✓$(NC) powerlevel10k"; \
	else \
		echo -e "  $(RED)✗$(NC) powerlevel10k (run: make deps)"; \
	fi
	@if [ -d "$(ZSH_COMPLETIONS_DIR)" ]; then \
		echo -e "  $(GREEN)✓$(NC) zsh-completions"; \
	else \
		echo -e "  $(RED)✗$(NC) zsh-completions (run: make deps)"; \
	fi
	@if [ -d "$(ZSH_SYNTAX_DIR)" ]; then \
		echo -e "  $(GREEN)✓$(NC) zsh-syntax-highlighting"; \
	else \
		echo -e "  $(RED)✗$(NC) zsh-syntax-highlighting (run: make deps)"; \
	fi
	@echo ""
	@echo -e "$(BLUE)Symlinks:$(NC)"
	@for file in $(SYMLINKS) $(DIR_SYMLINKS); do \
		dst="$(HOME_DIR)/$$file"; \
		if [ -L "$$dst" ]; then \
			echo -e "  $(GREEN)✓$(NC) $$file"; \
		elif [ -e "$$dst" ]; then \
			echo -e "  $(YELLOW)!$(NC) $$file (exists but not symlinked)"; \
		else \
			echo -e "  $(RED)✗$(NC) $$file (missing)"; \
		fi \
	done
	@echo ""
	@$(MAKE) --no-print-directory check-tools

# Check for recommended tools
check-tools:
	@echo -e "$(BLUE)Recommended Tools:$(NC)"
	@# Required
	@if command -v git >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) git"; \
	else \
		echo -e "  $(RED)✗$(NC) git (REQUIRED)"; \
	fi
	@if command -v zsh >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) zsh"; \
	else \
		echo -e "  $(RED)✗$(NC) zsh (REQUIRED)"; \
	fi
	@# Optional but recommended
	@if command -v fzf >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) fzf"; \
	else \
		echo -e "  $(YELLOW)○$(NC) fzf (optional - fuzzy finder)"; \
	fi
	@if command -v fd >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) fd"; \
	else \
		echo -e "  $(YELLOW)○$(NC) fd (optional - fast find)"; \
	fi
	@if command -v rg >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) rg (ripgrep)"; \
	else \
		echo -e "  $(YELLOW)○$(NC) rg (optional - ripgrep)"; \
	fi
	@if command -v lsd >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) lsd"; \
	else \
		echo -e "  $(YELLOW)○$(NC) lsd (optional - ls replacement)"; \
	fi
	@if command -v nvim >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) nvim"; \
	else \
		echo -e "  $(YELLOW)○$(NC) nvim (optional - neovim)"; \
	fi
	@if command -v emacs >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) emacs"; \
	else \
		echo -e "  $(YELLOW)○$(NC) emacs (optional)"; \
	fi
	@if command -v diff-so-fancy >/dev/null 2>&1; then \
		echo -e "  $(GREEN)✓$(NC) diff-so-fancy"; \
	else \
		echo -e "  $(YELLOW)○$(NC) diff-so-fancy (optional - git diff)"; \
	fi
	@# macOS-specific
	@if [ "$(OS)" = "Darwin" ]; then \
		if command -v gsed >/dev/null 2>&1; then \
			echo -e "  $(GREEN)✓$(NC) gsed (GNU sed)"; \
		else \
			echo -e "  $(YELLOW)○$(NC) gsed (optional - GNU sed for macOS)"; \
		fi; \
		if command -v trash >/dev/null 2>&1; then \
			echo -e "  $(GREEN)✓$(NC) trash"; \
		else \
			echo -e "  $(YELLOW)○$(NC) trash (optional - safe rm)"; \
		fi; \
	fi
	@echo ""
