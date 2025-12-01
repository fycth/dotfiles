# Dotfiles

Cross-platform dotfiles for macOS and Linux.

## Quick Start

```bash
git clone https://github.com/fycth/dotfiles ~/dotfiles
cd ~/dotfiles
make install
```

## Commands

| Command         | Description                              |
|-----------------|------------------------------------------|
| `make install`  | Full setup (clone deps + create symlinks)|
| `make check`    | Show what's installed vs missing         |
| `make deps`     | Clone git dependencies only              |
| `make symlinks` | Create symlinks only                     |
| `make update`   | Pull latest for git dependencies         |
| `make clean`    | Remove symlinks (keeps cloned repos)     |

## What Gets Installed

### Git Dependencies (cloned automatically)
- oh-my-zsh
- powerlevel10k theme
- zsh-completions
- zsh-syntax-highlighting

### Symlinked Configs
- Shell: `.zshrc`, `.zshenv.sh`, `.zlogin.sh`
- Git: `.gitconfig`, `.gitignore_global`, `.gitmessage`, `.git_template/`
- Editors: `.emacs.d/`, `.config/nvim/`
- Haskell: `.ghci`, `.hindent.yaml`, `.stylish-haskell.yaml`, `.stack/`
- Other: `.gemrc`, `.mvn/`

## Required Tools

- `git`
- `zsh`

## Recommended Tools

Run `make check` to see what's installed. Optional but recommended:

- `fzf` - fuzzy finder
- `fd` - fast find replacement
- `rg` (ripgrep) - fast grep replacement
- `lsd` - ls replacement
- `nvim` - neovim
- `emacs`
- `diff-so-fancy` - better git diffs

macOS-specific:
- `gsed` - GNU sed
- `trash` - safe rm replacement

## Re-running

The Makefile is re-entrant. Running `make install` multiple times is safe:
- Existing symlinks are skipped
- Already cloned repos are skipped
- Existing non-symlinked files are reported (won't overwrite)
