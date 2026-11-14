;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;; Keep backup files together, without hard-coding a home directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      version-control t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(xterm-mouse-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)

;; Share kills and yanks with the graphical desktop clipboard.
(setq select-enable-clipboard t)

;; macOS terminal frames do not have the GUI clipboard integration.
(when (and (eq system-type 'darwin)
           (executable-find "pbcopy")
           (executable-find "pbpaste"))
  (defvar my-terminal-last-clipboard nil
    "Last clipboard text exchanged with the macOS terminal clipboard.")
  (defun my-terminal-cut-to-clipboard (text &optional push)
    "Send TEXT to the macOS clipboard in a terminal frame.
PUSH is ignored."
    (unless (display-graphic-p)
      (let ((coding-system-for-write 'utf-8))
        (with-temp-buffer
          (insert text)
          (when (zerop (call-process-region (point-min) (point-max) "pbcopy"))
            (setq my-terminal-last-clipboard text))))))
  (defun my-terminal-paste-from-clipboard ()
    "Return the macOS clipboard contents in a terminal frame."
    (unless (display-graphic-p)
      (let ((coding-system-for-read 'utf-8))
        (with-temp-buffer
          (when (zerop (call-process "pbpaste" nil t nil))
            (let ((text (buffer-string)))
              (unless (or (equal text "")
                          (equal text my-terminal-last-clipboard))
                (setq my-terminal-last-clipboard text))))))))
  (unless (display-graphic-p)
    (setq interprogram-cut-function #'my-terminal-cut-to-clipboard
          interprogram-paste-function #'my-terminal-paste-from-clipboard)))

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package agent-shell
  :commands (agent-shell))

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)))

(use-package diff-hl
  :demand t
  :hook ((diff-hl-mode . diff-hl-flydiff-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;;; init.el ends here
