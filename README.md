# Emacs Configuration

A personal, modern Emacs configuration featuring Modus themes, lightweight completion (Vertico, Orderless, Marginalia, Consult, Corfu), built-in LSP (Eglot), and comprehensive Git tooling (Magit, Forge).

## Requirements

- Emacs 30 or later (relies on built-in `use-package`, `eglot`, `sqlite`)
- Git on `exec-path` (for Magit and version-control features)
- Optional: [JetBrains Mono](https://www.jetbrains.com/lp/mono/) font for graphical frames (`brew install --cask font-jetbrains-mono`)
- Optional: A [Nerd Font](https://www.nerdfonts.com/) (such as `Symbols Nerd Font`, e.g. `brew install --cask font-symbols-only-nerd-font`) for `all-the-icons-nerd-fonts` icon glyphs
- Internet connection on first launch to download packages (unpinned versions from MELPA / ELPA)
- [age](https://github.com/FiloSottile/age) 1.0 or later on `exec-path` for encrypted authentication (for example, `brew install age` on macOS). The `age` Emacs package is installed from MELPA.
- Optional: [ripgrep](https://github.com/BurntSushi/ripgrep) on `exec-path` for `consult-ripgrep`, and [Pandoc](https://pandoc.org/) for Markdown export and preview.

## Setup

1. If `~/.emacs.d` already exists, back up any files you need to keep.
2. Place this repository at `~/.emacs.d`. (Back up `~/.emacs` or `~/.emacs.el` if present, as Emacs prioritizes them).
3. Create an age identity and recipient before using authentication:

   ```sh
   mkdir -p ~/.config/age
   chmod 700 ~/.config/age
   age-keygen -o ~/.config/age/identity.txt
   age-keygen -y ~/.config/age/identity.txt > ~/.config/age/recipients.txt
   chmod 600 ~/.config/age/identity.txt
   ```

   Keep a secure backup of the identity: it is an unencrypted private key and anyone who can read it can decrypt the stores. Do not overwrite an existing identity. If migrating from an earlier configuration, move the existing `identity.txt` and `recipients.txt` from the Emacs configuration directory's `age/` subdirectory to `~/.config/age/` instead of generating a new key.
4. Start Emacs. `use-package` will download and compile missing packages automatically.

To migrate existing authentication, open `~/.authinfo.gpg` in Emacs, copy its contents into `~/.authinfo.age`, and save. This preserves Forge entries as well as the Google client credentials. Disable auto-save and backups for the plaintext buffer while copying. Keep the original until the new file and Forge access have been verified. If migrating from an earlier version of this configuration, move `~/.emacs.d/authinfo.age` to `~/.authinfo.age` instead. The configured `auth-sources` uses only the age file; other auth sources must be migrated too.

If an old `oauth2-auto.plist` exists but `oauth2-auto.age` does not, move the old file aside and authorize Google again. Verify synchronization before removing the old file.

If package installation fails, run `M-x package-refresh-contents` and restart Emacs.

> [!NOTE]
> On macOS 27, early startup sets `MACOSX_DEPLOYMENT_TARGET` to `27.0` if unset to work around a native-compilation issue in affected libgccjit builds. An existing value is respected and inherited by subprocesses.

## Features & Configuration

### Appearance & Display
- **Theme**: Terminal-only sessions use Modus Vivendi (dark). In graphical sessions, Modus Vivendi and Modus Operandi (light) follow the OS appearance: macOS checks on focus changes and after 30 seconds idle; other systems use [auto-dark](https://github.com/LionyxML/auto-dark-emacs) (Linux requires D-Bus). Themes are global to an Emacs process, so GUI and terminal frames in the same process cannot have independent themes.
- **Mode Line**: [doom-modeline](https://github.com/seagle0128/doom-modeline) without external icon font requirements.
- **Icons**: [all-the-icons](https://github.com/domtronn/all-the-icons.el) configured with [all-the-icons-nerd-fonts](https://github.com/mohkale/all-the-icons-nerd-fonts) (`all-the-icons-nerd-fonts-prefer`) to render icon glyphs using Nerd Fonts.
- **Typography**: Graphical frames use [JetBrains Mono](https://www.jetbrains.com/lp/mono/) at 14pt for the default and fixed-pitch faces, and Hiragino Sans for Japanese text, when those fonts are available. Otherwise the existing font settings are retained. Terminal frames retain the emulator's font.
- **Frame Size**: The size of the last graphical frame is saved on exit to `~/.emacs.d/frame-size.el` and restored before the initial frame is drawn on the next start. Terminal frames, child frames, and fullscreen or maximized frames are ignored, so the last normal size is kept when Emacs exits fullscreen.
- **Visual Aids**: Line numbers in programming/text buffers, matching parenthesis highlighting (`show-paren-mode`), current line highlighting (`global-hl-line-mode`), and nested delimiter coloring ([rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)).
- **Focused Writing**: [Olivetti](https://github.com/rnkn/olivetti) centers the text area in Markdown and Org buffers and hides line numbers while active. Toggle it in the current buffer with `M-x olivetti-mode`; the previous line-number setting is restored when it is disabled.
- **Mixed-pitch Text**: [mixed-pitch](https://github.com/jdtsmith/mixed-pitch) uses Avenir Next for prose in Markdown and Org buffers when available, falling back to the environment's variable-pitch font otherwise. Suitable elements remain fixed-pitch. Toggle it in the current buffer with `M-x mixed-pitch-mode`. Terminal frames use the terminal's font.

### Minibuffer & Completion
- **Vertical Minibuffer**: [Vertico](https://github.com/minad/vertico) displays candidates vertically.
- **Flexible Matching**: [Orderless](https://github.com/oantolin/orderless) filters candidates by multiple space-separated patterns. File names use `partial-completion`.
- **Annotations**: [Marginalia](https://github.com/minad/marginalia) displays candidate types, file sizes, and docstrings.
- **Search & Navigation**: [Consult](https://github.com/minad/consult) provides enhanced buffer switching, line search, `ripgrep`/`find`, imenu jump, and Xref integration.
- **Workspaces**: [tabspaces](https://codeberg.org/mclear-tools/tabspaces) groups buffers by tab. Switching projects with `C-x p p` or the dashboard opens or reuses a project workspace without prompting for another action. When a saved session exists, opening a project restores its supported buffers and window layout; otherwise it uses the normal project-opening behavior. Sessions are saved when closing a project tab, on Emacs exit, and after five minutes idle, under `~/.emacs.d/tabspaces-sessions/`. They are restored on demand, not at startup. Unsaved buffer contents and running processes are not preserved, and remote buffers are skipped during restore. `C-c TAB s` switches or creates a workspace, `C-c TAB o` opens a project in a workspace, and `C-c TAB d` closes one. `C-x b` lists buffers belonging to the current workspace (including shared buffers such as `*scratch*`); Consult's `o` narrowing shows buffers from other workspaces. Recent files and bookmarks remain available across workspaces. The workspace buffer list also affects other Consult commands such as `consult-project-buffer` and `consult-line-multi`.
- **Context Actions**: [Embark](https://github.com/oantolin/embark) and [Embark Consult](https://github.com/oantolin/embark) act on candidates or targets at point.
- **In-Buffer Completion**: [Corfu](https://github.com/minad/corfu) provides lightweight popup completion with documentation popups and candidate history (in graphical frames and terminal Emacs 31+).
- **Completion Backends**: [Cape](https://github.com/minad/cape) adds file paths and dabbrev buffer words to Corfu.
- **Key Discovery**: which-key shows hints for partial key sequences (built into Emacs).

### Editing
- **Region Expansion**: [expand-region](https://github.com/magnars/expand-region.el) semantically expands selections (`C-=`).
- **Move Text**: [drag-stuff](https://github.com/rejeep/drag-stuff.el) moves lines, words, or regions with `M-<up>`, `M-<down>`, `M-<left>`, and `M-<right>` (disabled in Org buffers to preserve Org's structure commands).
- **Multiple Cursors**: [multiple-cursors](https://github.com/magnars/multiple-cursors.el) edits multiple lines or matched occurrences simultaneously.
- **Repeatable Commands**: `repeat-mode` repeats commands such as `C-x o` with single keys.
- **EditorConfig**: Enforces project indentation and whitespace rules (built into Emacs).
- **Clipboard**: Seamless OS clipboard integration (including macOS terminal frames via `pbcopy`/`pbpaste`).

### Org
- **Capture & Agenda**: Built-in Org with `C-c c` for capture (task/note into `~/org/inbox.org`) and `C-c a` for the agenda. All `.org` files under `~/org` are used automatically.
- **Modern Styling**: [org-modern](https://github.com/minad/org-modern) renders bullets, tags, TODO keywords, and folded headings with cleaner glyphs, including in the agenda.
- **Markup Reveal**: [org-appear](https://github.com/awth13/org-appear) shows emphasis markers only while the cursor is on the text, so documents stay visually clean.
- **Editing Comforts**: Indented startup view, content folding, native syntax highlighting for source blocks, fast TODO selection, and done-time logging into the `LOGBOOK` drawer.
- **Google Calendar**: [org-gcal](https://github.com/kidd/org-gcal.el) synchronizes the primary calendar bidirectionally with `~/org/gcal.org`; OAuth credentials are read lazily from the age-encrypted auth source (manual sync only, no timer).
- **Japanese Holidays**: [japanese-holidays](https://github.com/emacs-jp/japanese-holidays) marks holidays in `M-x calendar` and includes them in the Org agenda via the diary.

### Git & Version Control
- **Git Interface**: [Magit](https://magit.vc/) for Git operations (`C-x g`).
- **Forge Integration**: [Forge](https://magit.vc/manual/forge/) allows viewing and managing GitHub/GitLab issues and pull requests inside Magit.
- **Diff Highlighting**: [diff-hl](https://github.com/dgutov/diff-hl) displays uncommitted changes in the window margin/fringe and refreshes with Magit.

### Development & Tools
- **LSP Client**: Built-in [Eglot](https://www.gnu.org/software/emacs/manual/html_node/eglot/) for Language Server Protocol integration (`M-x eglot`).
- **Project Tree**: [Treemacs](https://github.com/Alexander-Miller/treemacs) opens automatically when switching projects and can be toggled with `C-c t`. On project switches and when toggled from a project buffer, it synchronizes the current tab's tree to that project's root, replacing other Treemacs roots in that workspace. Its tab-bar integration keeps a separate tree per workspace, and [treemacs-nerd-icons](https://github.com/rainstormstudio/treemacs-nerd-icons) renders file and directory icons using Nerd Fonts. Treemacs workspace state is not persisted across restarts.
- **AI Agents**: [agent-shell](https://github.com/xenodium/agent-shell) for interacting with ACP-compatible AI agents (`M-x agent-shell`), using mixed-pitch text. Notifications report completed responses and permission requests outside the selected window.
- **Markdown**: [markdown-mode](https://jblevins.org/projects/markdown-mode/) with native syntax-highlighted code blocks and markup hiding. Generated HTML previews mirror the source path under `~/.emacs.d/markdown-preview/` instead of being written beside the Markdown file.

### System & Environment
- **Startup Dashboard**: [dashboard](https://github.com/emacs-dashboard/dashboard) shows recent files, projects, bookmarks, and today's Org agenda at startup. Use `M-x dashboard-open` to revisit it, `g` to refresh, and `RET` to open an item.
- **Notifications**: [alert](https://github.com/jwiegley/alert) uses macOS notifications on macOS and the minibuffer on other systems. `org-gcal` status notifications use the same setting.
- **Environment Variables**: [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) syncs shell `$PATH` and variables in graphical frames and daemon sessions.
- **History & Revert**: Persistent minibuffer history (`savehist-mode`), recent files (`recentf-mode`), cursor position restoration (`save-place-mode`), and automatic buffer reload on external file changes (`global-auto-revert-mode`).
- **Backups and Locks**: Backups and editing locks are kept in `backups/` and `locks/` under the configuration directory. Lock collision detection requires other Emacs instances editing the same files to use the same lock directory; it does not coordinate across different local directories or hosts.
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
| **Workspaces** | `C-c TAB s` | Switch or create a workspace |
| | `C-c TAB o` | Open a project in a workspace |
| | `C-c TAB d` | Close the current workspace |
| **Editing** | `C-=` | `er/expand-region` (expand selection semantically) |
| | `M-<up>` / `M-<down>` / `M-<left>` / `M-<right>` | `drag-stuff` (move line, word, or region; except in Org) |
| | `C-S-c C-S-c` | `mc/edit-lines` (edit all lines in selection) |
| | `C->` / `C-<` | `mc/mark-next-like-this` / `previous` |
| | `C-c C-<` | `mc/mark-all-like-this` |
| **Org** | `C-c a` | `org-agenda` (agenda for `~/org`) |
| | `C-c c t` / `C-c c n` / `C-c c a` | `org-capture` (task / note / appointment) |
| | `C-c c C` | Finish capture |
| | `C-c g f` | Fetch Google Calendar events into `gcal.org` |
| | `C-c g s` | Bidirectional Google Calendar / Org sync |
| | `C-c g p` | Post the appointment at point to Google Calendar |
| | `C-c l` | `org-store-link` (link to current location) |
| | `C-c C-e` | Export dispatcher (in Org buffer) |
| | `TAB` / `S-TAB` | Fold / unfold (in Org buffer) |
| **Git** | `C-x g` | `magit-status` |
| | `N` (in Magit) | `forge-pull` (fetch forge issues & PRs) |
| | `s` / `u` / `c c` | Stage / Unstage / Commit in Magit |
| **Web & Docs** | `C-c C-x C-m` | `markdown-toggle-markup-hiding` |
| **Tools** | `M-x eglot` | Start Language Server Protocol client |
| | `C-c t` | Toggle the Treemacs project tree |
| | `M-x agent-shell` | Open ACP AI agent shell |

## Additional Setup

### AI Agents (`agent-shell`)
Install and authenticate an agent separately (see [agent-shell installation](https://github.com/xenodium/agent-shell#agent-installation)). For Codex, ensure `codex-acp` is on `exec-path`. Run `M-x agent-shell`, select an installed agent, and interact (`RET` for newline, `C-c C-c` to submit, `C-c C-k` to interrupt). Transcripts, screenshots, and worktrees are kept under `~/.emacs.d/.agent-shell/` instead of each project directory.

### Org Files
Org data lives outside this repository in `~/org/`. The directory is scanned automatically for agenda files, and the first capture creates `~/org/inbox.org` with `Tasks` and `Notes` sections. Back up `~/org` separately if needed.

### Japanese Holidays
Japanese national holidays are calculated locally by `japanese-holidays` and replace the default `calendar-holidays`, so unrelated holidays such as Thanksgiving are not shown. They are highlighted in `M-x calendar` and appear as `Diary:` entries in the Org agenda (`org-agenda-include-diary`), including month view.

### Google Calendar (`org-gcal`)

1. In [Google Cloud Console](https://console.cloud.google.com/), create a project, enable the **Google Calendar API**, configure the **OAuth consent screen** (External), and create an **OAuth client ID** of type **Desktop app**. Add your own Google account as a test user (or publish the app to avoid token expiry).
2. Open `~/.authinfo.age` in Emacs (`C-x C-f`) and add the client ID and secret, then save to encrypt them with age:

   ```
   machine gcal-client-id password 1234567890-xxxxxxxx.apps.googleusercontent.com
   machine gcal-client-secret password GOCSPX-xxxxxxxxxxxxxxxx
   ```

3. Run `C-c g f` in Emacs. A message in the minibuffer asks you to log in, and your **external browser** opens for the first OAuth authorization. Grant access to your calendar, approve the "unverified app" warning if the consent screen is in Testing mode, then wait for `Completed event fetching` in the minibuffer before opening the agenda.

`org-gcal` stores fetched events in `~/org/gcal.org` (included in the agenda via the `~/org` scan). `C-c c a` captures a new appointment there with the required `:org-gcal:` drawer, and `C-c g p` sends the entry at point to Google; `C-c g s` runs a full bidirectional sync. Sync is manual only — nothing runs at startup or on a timer. Credentials are injected with `:before` advice on the upstream `org-gcal` commands, so `M-x org-gcal-sync` and automatic post/refile/delete hooks are covered too. `org-gcal` inserts events into a buffer without writing the file, so this configuration also saves `gcal.org` automatically after each update.

> [!IMPORTANT]
> OAuth tokens are kept in `~/.emacs.d/oauth2-auto.age` and sync tokens in `~/.emacs.d/persist/`. Both are excluded from Git. age.el uses temporary files for plaintext during encryption and decryption; it uses `/dev/shm` when available and the system temporary directory otherwise. The age private key is not passphrase-protected by this setup.
> A consent screen left in **Testing** makes refresh tokens expire after **7 days**; publish the app (and accept the unverified-app warning) for personal use to avoid this.
> `gcal.org` is managed by `org-gcal`. Do not hand-edit its `:ID:` / `:ETag:` properties, and keep personal notes in other files such as `inbox.org`.
> If nothing happens after `C-c g f`, check the minibuffer first: Emacs may be waiting for browser authorization; the sync is asynchronous and `gcal.org` stays empty until it finishes. If the external browser cannot reach the localhost callback, set `oauth2-auto-manually-auth` to `t` and paste the authorization code from the redirect URL instead.

### Language Servers (`Eglot`)
Install the language server for your language and ensure its executable is available on `exec-path`. Open a source file in its project and run `M-x eglot RET`. Use `M-.` to find definitions, `M-,` to return, `M-x completion-at-point` for completion, `M-x eglot-rename` to rename symbols, and `M-x eglot-code-actions` for code actions. To automate, add `eglot-ensure` to the major mode's hook. `eglot-autoshutdown` stops the server when its last buffer is closed, and `read-process-output-max` is raised to 1 MiB for LSP traffic.

## Files

- `early-init.el`: Early startup settings, including the macOS native-compilation workaround and restoration of the saved graphical frame size.
- `init.el`: Package, display, and backup settings.
- `custom.el`: Settings managed by Emacs Customize (auto-loaded).
- `LICENSE`: MIT License.
- `elpa/`, `eln-cache/`, `backups/`, `markdown-preview/`, `oauth2-auto.plist`, `persist/`, `frame-size.el`: Installed packages, compiled cache, backups, generated Markdown previews, org-gcal OAuth/sync tokens, and the saved graphical frame size (excluded from Git).
