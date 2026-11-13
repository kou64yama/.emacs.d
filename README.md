# Emacs Configuration

This is a personal Emacs configuration with the Dracula theme and packages that improve minibuffer completion and search.

## Requirements

- Emacs 29 or later (for the built-in `use-package`)
- An internet connection to download packages on the first launch

The local Emacs version is 31.1. Package versions are not pinned.

## Setup

1. If `~/.emacs.d` already exists, back up any files you need to keep.
2. Place this repository at `~/.emacs.d`. Emacs gives precedence to `~/.emacs` and `~/.emacs.el` if they exist, so back them up as needed.
3. Start Emacs. `use-package` will download any missing packages.

If package installation fails, check your network connection, run `M-x package-refresh-contents`, and restart Emacs.

## Configuration

- [Dracula theme](https://github.com/dracula/emacs): Color theme.
- [Vertico](https://github.com/minad/vertico): Displays minibuffer completion candidates vertically.
- [Orderless](https://github.com/oantolin/orderless): Filters candidates using multiple space-separated patterns. File name completion uses `partial-completion`.
- [Marginalia](https://github.com/minad/marginalia): Adds annotations, such as descriptions, to completion candidates.
- Line numbers and matching parentheses are displayed. The menu bar, tool bar, and scroll bar are hidden when available.
- Backups are stored in `backups/` under the Emacs configuration directory. Up to two old versions and six new versions are retained.

Use `M-x` to run a command and see completion candidates. In Vertico, use `C-n` and `C-p` to move between candidates and `RET` to select one.

## Files

- `init.el`: Package, display, and backup settings.
- `custom.el`: Settings saved by Emacs Customize, loaded from `init.el`.
- `elpa/`, `eln-cache/`, and `backups/`: Directories for installed packages, compiled files, and backups. These are excluded from Git.
