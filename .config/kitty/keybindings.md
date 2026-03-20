# Kitty Keybindings Reference

macOS uses Cmd, Linux uses Ctrl+Shift.

## Tabs

| macOS | Linux | Action |
|-------|-------|--------|
| `Cmd+T` | `Ctrl+Shift+T` | New tab |
| `Cmd+Shift+]` | `Ctrl+Shift+Right` | Next tab |
| `Cmd+Shift+[` | `Ctrl+Shift+Left` | Previous tab |
| `Cmd+1..9` | `Ctrl+Shift+1..9` | Jump to tab by number |

## Copy & Paste (built-in defaults)

| macOS | Linux | Action |
|-------|-------|--------|
| `Cmd+C` | `Ctrl+Shift+C` | Copy selection |
| `Cmd+V` | `Ctrl+Shift+V` | Paste from clipboard |
| Select text | Select text | Auto-copies to clipboard (`copy_on_select`) |

## Scrollback (built-in defaults)

| macOS | Linux | Action |
|-------|-------|--------|
| `Cmd+Up` | `Ctrl+Shift+Up` | Scroll up |
| `Cmd+Down` | `Ctrl+Shift+Down` | Scroll down |
| `Cmd+Page_Up` | `Shift+Page_Up` | Scroll page up |
| `Cmd+Page_Down` | `Shift+Page_Down` | Scroll page down |
| `Cmd+Home` | `Ctrl+Shift+Home` | Scroll to top |
| `Cmd+End` | `Ctrl+Shift+End` | Scroll to bottom |

## Font Size (built-in defaults)

| macOS | Linux | Action |
|-------|-------|--------|
| `Cmd+=` | `Ctrl+Shift+=` | Increase font size |
| `Cmd+-` | `Ctrl+Shift+-` | Decrease font size |
| `Cmd+0` | `Ctrl+Shift+Backspace` | Reset font size |

## How Copy/Paste Works Over SSH + tmux

The clipboard chain: remote app → tmux → kitty → system clipboard.

Three pieces make this work:

1. **tmux** has `set -g set-clipboard on` and `allow-passthrough on`, so it
   forwards OSC 52 clipboard escape sequences to the terminal
2. **kitty** has `clipboard_control write-clipboard write-primary read-clipboard
   read-primary`, so it accepts those sequences and writes to the system clipboard
3. **kitty** has `copy_on_select clipboard`, so any text you select is
   immediately copied

### Selecting text inside tmux or vim (mouse grabbed)

When tmux or vim has grabbed the mouse, normal selection sends events to the
app, not kitty. To select text with kitty instead:

- Hold **Shift** and drag to select — this bypasses tmux/vim and lets kitty
  handle the selection directly
- The selected text is auto-copied to clipboard (`copy_on_select`)
- Paste with `Cmd+V` / `Ctrl+Shift+V`

### Using tmux's own copy mode

- Scroll up with mouse wheel inside tmux — this enters tmux copy mode
- Select text by clicking and dragging (tmux handles it)
- tmux sends the selection to kitty via OSC 52 → system clipboard

### Summary

| Scenario | How to select | How it reaches clipboard |
|----------|--------------|------------------------|
| Local shell (no tmux) | Click and drag | kitty `copy_on_select` |
| Inside tmux | Mouse wheel to scroll, then drag | tmux OSC 52 → kitty |
| Inside tmux (override) | Shift+drag | kitty `copy_on_select` |
| Inside vim/emacs over SSH | Shift+drag | kitty `copy_on_select` |
