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
- Other: `.mvn/`

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
- `ag` - the silver searcher

macOS-specific:
- `gsed` - GNU sed
- `trash` - safe rm replacement

## Re-running

The Makefile is re-entrant. Running `make install` multiple times is safe:
- Existing symlinks are skipped
- Already cloned repos are skipped
- Existing non-symlinked files are reported (won't overwrite)

## Recommended software

- The Universal Document Viewer: [Okuklar](https://okular.kde.org/)
- Music Player: [DeaDBeeF](https://github.com/DeaDBeeF-Player/deadbeef)
- Terminal: [kitty](https://sw.kovidgoyal.net/kitty/)
- Font: [Maple](https://font.subf.dev/en/)

