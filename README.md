# Emacs Configuration

This is a personal Emacs configuration with the Dracula theme and packages that improve minibuffer completion and search.

## Requirements

- Emacs 29 or later (for the built-in `use-package`)
- Git, available on Emacs's `exec-path` (for Magit and Git repositories)
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
- [Eglot](https://www.gnu.org/software/emacs/manual/html_node/eglot/): Built-in Language Server Protocol client, started manually for a project.
- [Magit](https://magit.vc/): Git interface; open the current repository with `C-x g`.
- [markdown-mode](https://jblevins.org/projects/markdown-mode/): Edits `.md` and `.markdown` files; `README.md` uses GitHub-flavored Markdown mode.
- [diff-hl](https://github.com/dgutov/diff-hl): Highlights version-controlled changes, including unsaved edits, at the side of file buffers and refreshes after Magit operations. In terminals, indicators use the window margin.
- Line numbers and matching parentheses are displayed. The menu bar, tool bar, and scroll bar are hidden when available.
- Backups are stored in `backups/` under the Emacs configuration directory. Up to two old versions and six new versions are retained.

Use `M-x` to run a command and see completion candidates. In Vertico, use `C-n` and `C-p` to move between candidates and `RET` to select one.

In a Magit status buffer, use `TAB` to expand a section, `s` to stage the change at point, `u` to unstage it, and `c c` to start a commit. Finish the commit message with `C-c C-c`. See the [Magit getting started guide](https://docs.magit.vc/magit/Getting-Started.html) for more commands.

### Language servers

Install a language server for the language you use, and make sure Emacs can find its executable on `exec-path`. Eglot includes default commands for many servers; see `M-x describe-variable RET eglot-server-programs RET` and the [Eglot manual](https://www.gnu.org/software/emacs/manual/html_node/eglot/Setting-Up-LSP-Servers.html) for server configuration. Server installation and versions are managed separately from this configuration.

Open a source file in its project and run `M-x eglot RET`. Once connected, use `M-.` to find a definition, `M-,` to return, and `M-x completion-at-point RET` for completion. Commands such as `M-x eglot-rename RET` and `M-x eglot-code-actions RET` are available when supported by the server. To enable Eglot automatically for a language, add `eglot-ensure` to that major mode's hook after installing its server.

## Files

- `init.el`: Package, display, and backup settings.
- `custom.el`: Settings saved by Emacs Customize, loaded from `init.el`.
- `elpa/`, `eln-cache/`, and `backups/`: Directories for installed packages, compiled files, and backups. These are excluded from Git.
