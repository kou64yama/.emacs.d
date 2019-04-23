;;; init.el -- Emacs Configuration File.
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.
;;
;; This source code is licensed unter the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(when (file-exists-p (setq custom-file (locate-user-emacs-file "custom.el")))
  (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpha" . "https://melpa.org/packages/"))
(package-initialize)

(condition-case nil
    (require 'use-package)
  (file-error
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

;;
;; Benchmark
;; -----------------------------------------------------------------------------

(use-package benchmark-init
  ;; https://github.com/dholm/benchmark-init-el
  :ensure
  :init (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;
;; Statistics
;; -----------------------------------------------------------------------------

(use-package keyfreq
  ;; https://github.com/dacap/keyfreq
  :ensure
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;;
;; Input Method
;; -----------------------------------------------------------------------------

(use-package ddskk
  ;; http://openlab.jp/skk/
  :ensure
  :bind (("C-x C-j" . ssk-mode))
  :init
  (setq default-input-method "japanese-skk"))

;;
;; Dashboard
;; -----------------------------------------------------------------------------

(use-package dashboard
  ;; https://github.com/rakanalh/emacs-dashboard
  :ensure
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))
        show-week-agenda-p t))

;;
;; Appearance
;; -----------------------------------------------------------------------------

(use-package emacs
  :config
  (global-hl-line-mode)
  (menu-bar-mode -1))

(use-package emacs
  :if window-system
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(width . 100))
  (add-to-list 'default-frame-alist '(height . 36)))

(use-package spacemacs-theme
  :ensure
  :defer
  :init
  (load-theme 'spacemacs-dark t))

(use-package smart-mode-line-powerline-theme
  ;; https://github.com/Malabarba/smart-mode-line
  :ensure
  :init
  (setq sml/theme 'powerline)
  (sml/setup))

(use-package mode-icons
  ;; http://projects.ryuslash.org/mode-icons/
  :ensure
  :if window-system
  :init
  (mode-icons-mode))

(use-package emojify
  ;; https://github.com/iqbalansari/emacs-emojify
  :ensure
  :if window-system
  :init
  (global-emojify-mode))

(use-package dimmer
  :ensure
  :init
  (dimmer-mode)
  :config
  (setq dimmer-fraction .5))

;;
;; Font
;; -----------------------------------------------------------------------------
;;
;; Grumpy wizards make toxic brew for the evil Queen and Jack.
;;
;; あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、
;; うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。
;;

(use-package emacs
  :if window-system
  :config
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 128)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))

;;
;; Minibuffer
;; -----------------------------------------------------------------------------

(use-package ivy
  ;; https://github.com/abo-abo/swiper#ivy
  :ensure
  :bind (("C-c c-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :init
  (ivy-mode)
  :config
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(use-package counsel
  ;; https://github.com/abo-abo/swiper#counsel
  :ensure
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

;;
;; Editor
;; -----------------------------------------------------------------------------

(use-package emacs
  :config
  (column-number-mode)
  (setq-default indent-tabs-mode nil)
  (setq backup-inhibited t
        inhibit-startup-message t))

(use-package editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  :ensure
  :init
  (editorconfig-mode))

(use-package expand-region
  ;; https://github.com/magnars/expand-region.el
  :ensure
  :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package drag-stuff
  ;; https://github.com/rejeep/drag-stuff.el
  :ensure
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys))

(use-package smartparens
  ;; https://github.com/Fuco1/smartparens
  :ensure
  :init
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Org Mode
;; -----------------------------------------------------------------------------

(use-package org
  :config
  (setq org-directory "~/agenda"
        org-agenda-files '("~/agenda")))

;;
;; History
;; -----------------------------------------------------------------------------

(use-package undo-tree
  ;; https://www.emacswiki.org/emacs/UndoTree
  :ensure
  :init
  (global-undo-tree-mode))

(use-package undohist
  ;; https://github.com/m2ym/undohist-el
  :ensure
  :commands (undohist-initialize)
  :init
  (undohist-initialize)
  :config
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

;;
;; Project
;; -----------------------------------------------------------------------------

(use-package projectile
  ;; https://projectile.readthedocs.io/en/latest/
  :ensure
  :init
  (projectile-mode)
  :config
  (setq projectile-globally-ignored-directories
        (append '("elpa" "node_modules" "vendor")
                projectile-globally-ignored-directories)))

(use-package treemacs
  :ensure
  :bind (("M-0" . treemacs-select-window)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t t" . treemacs)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :ensure
  :after treemacs projectile)

(use-package treemacs-magit
  :ensure
  :after treemacs magit)

;;
;; Autocomplete
;; -----------------------------------------------------------------------------

(use-package company
  ;; https://company-mode.github.io
  :ensure
  :init
  (global-company-mode))

(use-package company-lsp
  :ensure
  :commands company-lsp)

;;
;; Syntax checker
;; -----------------------------------------------------------------------------

(use-package flycheck
  :ensure
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'symbols))

;;
;; Search
;; -----------------------------------------------------------------------------

(use-package swiper
  :ensure
  :bind (("C-s" . swiper)))

;;
;; Git
;; -----------------------------------------------------------------------------

(use-package magit
  :ensure
  :bind (("C-x g" . magit-status)))

(use-package gitconfig-mode :ensure)
(use-package gitignore-mode :ensure)
(use-package gitattributes-mode :ensure)

(use-package git-gutter-fringe
  :ensure
  :if window-system
  :init
  (global-git-gutter-mode)
  :config
  (fringe-helper-define 'git-gutter-fr:added '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:deleted '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:modified '(top repeat) "xxxx...."))

;;
;; Slack
;; -----------------------------------------------------------------------------

(use-package slack
  :ensure
  :commands slack-start
  :init
  (when (file-exists-p "~/.emacs.d/slack.el")
    (load-file "~/.emacs.d/slack.el")))

;;
;; LSP
;; -----------------------------------------------------------------------------

(use-package lsp-mode
  :ensure
  :commands lsp)

(use-package dap-mode
  :ensure
  :after lsp
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

;;
;; Language
;; -----------------------------------------------------------------------------

(use-package json-mode :ensure)
(use-package typescript-mode :ensure)
(use-package yaml-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package gradle-mode :ensure)
(use-package groovy-mode :ensure)
(use-package kotlin-mode :ensure)
(use-package adoc-mode :ensure)

;;; init.el ends here
