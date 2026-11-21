# Emacs Configuration

This is a personal Emacs configuration with the Catppuccin theme and packages that improve minibuffer completion and search.

## Requirements

- Emacs 29 or later (for the built-in `use-package`)
- Git, available on Emacs's `exec-path` (for Magit and Git repositories)
- [JetBrains Mono](https://www.jetbrains.com/lp/mono/), for graphical frames (`brew install --cask font-jetbrains-mono`)
- An internet connection to download packages on the first launch

The local Emacs version is 31.1. Package versions are not pinned.

## Setup

1. If `~/.emacs.d` already exists, back up any files you need to keep.
2. Place this repository at `~/.emacs.d`. Emacs gives precedence to `~/.emacs` and `~/.emacs.el` if they exist, so back them up as needed.
3. Start Emacs. `use-package` will download any missing packages.

If package installation fails, check your network connection, run `M-x package-refresh-contents`, and restart Emacs.

On macOS 27, early startup sets `MACOSX_DEPLOYMENT_TARGET` to `27.0` if it is unset, working around a native-compilation error in affected libgccjit builds. An existing value is respected. The setting is inherited by subprocesses started by Emacs.

## Configuration

- [Catppuccin theme](https://github.com/catppuccin/emacs): Color theme configured with the Mocha flavor.
- Graphical frames use [JetBrains Mono](https://www.jetbrains.com/lp/mono/) at 14pt for Latin text and Hiragino Sans for Japanese text, including `fixed-pitch` text such as code blocks. Terminal frames keep the font configured in the terminal emulator.
- [doom-modeline](https://github.com/seagle0128/doom-modeline): Displays buffer and version-control information in the mode line. Icons are disabled so no additional font is needed. The package and its dependencies are installed from MELPA.
- [Vertico](https://github.com/minad/vertico): Displays minibuffer completion candidates vertically.
- [Orderless](https://github.com/oantolin/orderless): Filters candidates using multiple space-separated patterns. File name completion uses `partial-completion`.
- [Marginalia](https://github.com/minad/marginalia): Adds annotations, such as descriptions, to completion candidates.
- [Consult](https://github.com/minad/consult): Switch buffers with `C-x b` and search lines with `M-s l`.
- [Embark](https://github.com/oantolin/embark): Act on a candidate or the item at point with `C-.`, use the default action with `C-;`, and view bindings with `C-h B`. Embark Consult supports exporting Consult search results.
- EditorConfig: Applies project `.editorconfig` settings. Emacs 30 and later include it; Emacs 29 installs the [EditorConfig package](https://github.com/editorconfig/editorconfig-emacs).
- [Corfu](https://github.com/minad/corfu): Shows completion candidates while typing, including candidates from Eglot. On terminal Emacs 29 and 30, standard completion remains in use; terminal Emacs 31 and graphical Emacs use Corfu.
- [Eglot](https://www.gnu.org/software/emacs/manual/html_node/eglot/): Built-in Language Server Protocol client, started manually for a project.
- [Magit](https://magit.vc/): Git interface; open the current repository with `C-x g`.
- [Forge](https://magit.vc/manual/forge/): Magit interface for Git forges (such as GitHub) to view and manage issues and pull requests.
- [agent-shell](https://github.com/xenodium/agent-shell): An Emacs interface for ACP-compatible AI agents, started with `M-x agent-shell`.
- [markdown-mode](https://jblevins.org/projects/markdown-mode/): Edits `.md` and `.markdown` files; `README.md` uses GitHub-flavored Markdown mode.
- [diff-hl](https://github.com/dgutov/diff-hl): Highlights version-controlled changes, including unsaved edits, at the side of file buffers and refreshes after Magit operations. In terminals, indicators use the window margin.
- Built-in EWW browser and `browse-url` open web pages in Emacs using DuckDuckGo search and auto-rename buffers per page. Media-heavy sites (YouTube) and GitHub issues/PRs open directly in the system's default browser; press `&` in EWW to view the current page externally. Global bindings `C-c w w` starts EWW and `C-c w s` searches for selected text.
- Line numbers are enabled in programming and text buffers (`prog-mode`, `text-mode`, and their derivatives), but not in shells or terminals. The current line and matching parentheses are highlighted. The menu bar, tool bar, and scroll bar are hidden when available.
- Minibuffer history is saved between sessions. Files changed externally are automatically reloaded when their buffers have no unsaved edits.
- Mouse clicks, dragging, and wheel scrolling are enabled in xterm-compatible terminals. Terminal text selection may require holding Shift; terminal multiplexers may need additional mouse configuration.
- Kills and copies are shared with the OS clipboard, and `C-y` can paste text copied in other applications. In a macOS terminal, this uses the system `pbcopy` and `pbpaste` commands. Other terminal environments require their own clipboard integration.
- Backups are stored in `backups/` under the Emacs configuration directory. Up to two old versions and six new versions are retained.

Use `M-x` to run a command and see completion candidates. In Vertico, use `C-n` and `C-p` to move between candidates and `RET` to select one.

In a Magit status buffer, use `TAB` to expand a section, `s` to stage the change at point, `u` to unstage it, and `c c` to start a commit. Finish the commit message with `C-c C-c`. See the [Magit getting started guide](https://docs.magit.vc/magit/Getting-Started.html) for more commands. In a repository hosted on GitHub or another supported forge, use `N` in Magit to fetch topics with `forge-pull` and view issues or pull requests.

### AI agents

Install and authenticate an agent separately, following the [agent-shell agent setup instructions](https://github.com/xenodium/agent-shell#agent-installation). Some agents also require an ACP adapter; for example, Codex requires `codex-acp`. Make sure the relevant executable is available on Emacs's `exec-path`. Agent and adapter versions are managed separately from this configuration.

From a project, run `M-x agent-shell` and select an installed agent. In the agent buffer, `RET` submits a prompt, `M-J` inserts a newline, and `C-c C-c` interrupts the current operation. Available capabilities and authentication depend on the chosen agent.

### Language servers

Install a language server for the language you use, and make sure Emacs can find its executable on `exec-path`. Eglot includes default commands for many servers; see `M-x describe-variable RET eglot-server-programs RET` and the [Eglot manual](https://www.gnu.org/software/emacs/manual/html_node/eglot/Setting-Up-LSP-Servers.html) for server configuration. Server installation and versions are managed separately from this configuration.

Open a source file in its project and run `M-x eglot RET`. Once connected, use `M-.` to find a definition, `M-,` to return, and `M-x completion-at-point RET` for completion. Commands such as `M-x eglot-rename RET` and `M-x eglot-code-actions RET` are available when supported by the server. To enable Eglot automatically for a language, add `eglot-ensure` to that major mode's hook after installing its server.

## Files

- `early-init.el`: Early startup settings, including the macOS native-compilation workaround.
- `init.el`: Package, display, and backup settings.
- `custom.el`: Settings saved by Emacs Customize, loaded from `init.el`.
- `elpa/`, `eln-cache/`, and `backups/`: Directories for installed packages, compiled files, and backups. These are excluded from Git.
