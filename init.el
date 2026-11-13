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
(global-display-line-numbers-mode 1)
(show-paren-mode 1)

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

;;; init.el ends here
