# Repository Guidelines

## Project Structure & Module Organization

This repository is a personal Emacs configuration. `init.el` configures package archives, packages, display options, completion, and backups. `custom.el` contains settings maintained by Emacs Customize and is loaded by `init.el`. `README.md` documents installation and user-facing behavior. There are currently no separate source, asset, or test directories.

Installed packages and local state, including `elpa/`, `eln-cache/`, and `backups/`, are ignored by Git. Do not commit generated files, backups, or machine-specific state.

## Development & Verification

Emacs 29 or later is required. Start Emacs with this repository installed as its configuration directory; on first launch, `use-package` downloads missing packages from configured archives. Package versions are not pinned, so installation requires network access and results can vary over time.

After editing, restart Emacs and check for startup errors. Exercise affected behavior interactively: for completion changes, run `M-x`, navigate candidates with `C-n` and `C-p`, and select one with `RET`. For package installation problems, run `M-x package-refresh-contents` and restart.

## Coding Style & Configuration

Write Emacs Lisp using the existing indentation and lowercase, hyphen-separated symbol names. Keep lexical binding enabled in Lisp file headers. Group package configuration in `use-package` declarations and explain non-obvious settings with concise comments. Resolve configuration paths relative to `user-emacs-directory` rather than hard-coding a home directory. Prefer editing `init.el` for hand-written configuration; treat `custom.el` as Customize-managed data.

## Testing Guidelines

There is no automated test suite or stated coverage requirement. Verify startup and the behavior affected by each change on an available supported Emacs version. Document any untested environments or assumptions in the pull request.

## Commits & Pull Requests

Recent commits use concise Conventional Commit prefixes, such as `feat:`, `docs:`, and `chore:`. Describe the change in the imperative. For Codex-created commits, include `Co-authored-by: codex <codex@openai.com>`. Pull requests should explain the purpose and user-visible effects, link relevant issues, and summarize verification; include screenshots when visual changes benefit from them. Update `README.md` when setup or behavior changes.
