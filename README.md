# Emacs Configuration

A personal, modern Emacs configuration featuring the Catppuccin theme, lightweight completion (Vertico, Orderless, Marginalia, Consult, Corfu), built-in LSP (Eglot), and comprehensive Git tooling (Magit, Forge).

## Requirements

- Emacs 29 or later (local version is 31.1; relies on built-in `use-package`, `eglot`, `sqlite`)
- Git on `exec-path` (for Magit and version-control features)
- [JetBrains Mono](https://www.jetbrains.com/lp/mono/) font for graphical frames (`brew install --cask font-jetbrains-mono`)
- Internet connection on first launch to download packages (unpinned versions from MELPA / ELPA)

## Setup

1. If `~/.emacs.d` already exists, back up any files you need to keep.
2. Place this repository at `~/.emacs.d`. (Back up `~/.emacs` or `~/.emacs.el` if present, as Emacs prioritizes them).
3. Start Emacs. `use-package` will download and compile missing packages automatically.

If package installation fails, run `M-x package-refresh-contents` and restart Emacs.

> [!NOTE]
> On macOS 27, early startup sets `MACOSX_DEPLOYMENT_TARGET` to `27.0` if unset to work around a native-compilation issue in affected libgccjit builds. An existing value is respected and inherited by subprocesses.

## Features & Configuration

### Appearance & Display
- **Theme**: [Catppuccin theme](https://github.com/catppuccin/emacs) with the Mocha flavor.
- **Mode Line**: [doom-modeline](https://github.com/seagle0128/doom-modeline) without external icon font requirements.
- **Typography**: [JetBrains Mono](https://www.jetbrains.com/lp/mono/) at 14pt (Latin) and Hiragino Sans (Japanese) in graphical frames. Terminal frames retain the emulator's font.
- **Visual Aids**: Line numbers in programming/text buffers, matching parenthesis highlighting (`show-paren-mode`), current line highlighting (`global-hl-line-mode`), and nested delimiter coloring ([rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)).

### Minibuffer & Completion
- **Vertical Minibuffer**: [Vertico](https://github.com/minad/vertico) displays candidates vertically.
- **Flexible Matching**: [Orderless](https://github.com/oantolin/orderless) filters candidates by multiple space-separated patterns. File names use `partial-completion`.
- **Annotations**: [Marginalia](https://github.com/minad/marginalia) displays candidate types, file sizes, and docstrings.
- **Search & Navigation**: [Consult](https://github.com/minad/consult) provides enhanced buffer switching, line search, `ripgrep`/`find`, imenu jump, and Xref integration.
- **Context Actions**: [Embark](https://github.com/oantolin/embark) and [Embark Consult](https://github.com/oantolin/embark) act on candidates or targets at point.
- **In-Buffer Completion**: [Corfu](https://github.com/minad/corfu) provides lightweight popup completion (in graphical frames and terminal Emacs 31+).
- **Completion Backends**: [Cape](https://github.com/minad/cape) adds file paths and dabbrev buffer words to Corfu.
- **Key Discovery**: which-key shows hints for partial key sequences (built-in on Emacs 30+; installs [which-key](https://github.com/justbur/emacs-which-key) on Emacs 29).

### Editing
- **Region Expansion**: [expand-region](https://github.com/magnars/expand-region.el) semantically expands selections (`C-=`).
- **Multiple Cursors**: [multiple-cursors](https://github.com/magnars/multiple-cursors.el) edits multiple lines or matched occurrences simultaneously.
- **EditorConfig**: Enforces project indentation and whitespace rules (built-in on Emacs 30+; installs [EditorConfig](https://github.com/editorconfig/editorconfig-emacs) on Emacs 29).
- **Clipboard**: Seamless OS clipboard integration (including macOS terminal frames via `pbcopy`/`pbpaste`).

### Git & Version Control
- **Git Interface**: [Magit](https://magit.vc/) for Git operations (`C-x g`).
- **Forge Integration**: [Forge](https://magit.vc/manual/forge/) allows viewing and managing GitHub/GitLab issues and pull requests inside Magit.
- **Diff Highlighting**: [diff-hl](https://github.com/dgutov/diff-hl) displays uncommitted changes in the window margin/fringe and refreshes with Magit.

### Development & Tools
- **LSP Client**: Built-in [Eglot](https://www.gnu.org/software/emacs/manual/html_node/eglot/) for Language Server Protocol integration (`M-x eglot`).
- **AI Agents**: [agent-shell](https://github.com/xenodium/agent-shell) for interacting with ACP-compatible AI agents (`M-x agent-shell`).
- **Markdown**: [markdown-mode](https://jblevins.org/projects/markdown-mode/) with native syntax-highlighted code blocks and markup hiding.
- **Web Browser**: Built-in [EWW](https://www.gnu.org/software/emacs/manual/html_node/emacs/EWW.html) with DuckDuckGo search, auto-renamed buffers, and automatic external browser routing for media-heavy sites and GitHub PRs.

### System & Environment
- **Environment Variables**: [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) syncs shell `$PATH` and variables in graphical frames and daemon sessions.
- **History & Revert**: Persistent minibuffer history (`savehist-mode`), recent files (`recentf-mode`), and automatic buffer reload on external file changes (`global-auto-revert-mode`).
- **Backups**: Centralized in `backups/` under the configuration directory with revision retention.
- **Mouse Support**: Terminal mouse click, drag, and scrolling enabled (`xterm-mouse-mode`).

## Keybindings Quick Reference

| Category | Keybinding | Command / Function |
| :--- | :--- | :--- |
| **Minibuffer** | `C-x b` | `consult-buffer` (switch buffer & recent files) |
| | `M-s l` | `consult-line` (search lines in buffer) |
| | `M-s r` | `consult-ripgrep` (search project with ripgrep) |
| | `M-s d` | `consult-find` (find file in project) |
| | `M-g g` | `consult-goto-line` (jump to line with preview) |
| | `M-g i` | `consult-imenu` (jump to symbol / heading) |
| | `M-y` | `consult-yank-pop` (browse kill ring history) |
| | `C-.` / `C-;` | `embark-act` / `embark-dwim` |
| | `C-h B` | `embark-bindings` |
| **Editing** | `C-=` | `er/expand-region` (expand selection semantically) |
| | `C-S-c C-S-c` | `mc/edit-lines` (edit all lines in selection) |
| | `C->` / `C-<` | `mc/mark-next-like-this` / `previous` |
| | `C-c C-<` | `mc/mark-all-like-this` |
| **Git** | `C-x g` | `magit-status` |
| | `N` (in Magit) | `forge-pull` (fetch forge issues & PRs) |
| | `s` / `u` / `c c` | Stage / Unstage / Commit in Magit |
| **Web & Docs** | `C-c w w` | `eww` (open EWW browser) |
| | `C-c w s` | `eww-search-words` (search selected text) |
| | `C-c C-x C-m` | `markdown-toggle-markup-hiding` |
| **Tools** | `M-x eglot` | Start Language Server Protocol client |
| | `M-x agent-shell` | Open ACP AI agent shell |

## Additional Setup

### AI Agents (`agent-shell`)
Install and authenticate an agent separately (see [agent-shell installation](https://github.com/xenodium/agent-shell#agent-installation)). For Codex, ensure `codex-acp` is on `exec-path`. Run `M-x agent-shell`, select an installed agent, and interact (`RET` to submit, `M-J` for newline, `C-c C-c` to interrupt).

### Language Servers (`Eglot`)
Install the language server for your language and ensure its executable is available on `exec-path`. Open a source file in its project and run `M-x eglot RET`. Use `M-.` to find definitions, `M-,` to return, `M-x completion-at-point` for completion, `M-x eglot-rename` to rename symbols, and `M-x eglot-code-actions` for code actions. To automate, add `eglot-ensure` to the major mode's hook.

## Files

- `early-init.el`: Early startup settings, including the macOS native-compilation workaround.
- `init.el`: Package, display, and backup settings.
- `custom.el`: Settings managed by Emacs Customize (auto-loaded).
- `LICENSE`: MIT License.
- `elpa/`, `eln-cache/`, `backups/`: Directories for installed packages, compiled cache, and backups (excluded from Git).
