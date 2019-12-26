;;; init.el -- Emacs Configuration File.
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.
;;
;; This source code is licensed unter the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq user-emacs-directory
      (expand-file-name (format "local/%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

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

;; https://github.com/dholm/benchmark-init-el
(use-package benchmark-init
  :ensure
  :init (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;
;; Statistics
;; -----------------------------------------------------------------------------

;; https://github.com/dacap/keyfreq
(use-package keyfreq
  :ensure
  :hook
  (after-init . keyfreq-mode)
  (after-init . keyfreq-autosave-mode))

;;
;; Environment
;; -----------------------------------------------------------------------------

(use-package exec-path-from-shell
  :ensure
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(use-package direnv
  :ensure
  :hook
  (after-init . direnv-mode))

;;
;; Input Method
;; -----------------------------------------------------------------------------

;; http://openlab.jp/skk/
(use-package ddskk
  :ensure
  :bind (("C-x C-j" . skk-mode))
  :custom
  (default-input-method "japanese-skk"))

;;
;; Dashboard
;; -----------------------------------------------------------------------------

;; https://github.com/rakanalh/emacs-dashboard
(use-package dashboard
  :ensure
  :custom
  (dashboard-items '((recents . 15)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-startup-banner 1)
  :hook
  (after-init . dashboard-setup-startup-hook))

;;
;; Appearance
;; -----------------------------------------------------------------------------

(use-package emacs
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-display-line-numbers-mode))

(use-package emacs
  :if (display-graphic-p)
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(width . 100))
  (add-to-list 'default-frame-alist '(height . 36))
  (setq-default indicate-empty-lines t
                indicate-buffer-boundaries '((top . nil) (bottom . right) (down . right))))

(use-package emacs
  :if (not (display-graphic-p))
  :config
  (menu-bar-mode -1))

(use-package doom-themes
  :ensure
  :init
  (load-theme 'doom-peacock t))

(use-package doom-modeline
  :ensure
  :hook
  (after-init . doom-modeline-mode))

;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :ensure
  :if (display-graphic-p)
  :hook
  (after-init . global-emojify-mode))

;; https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :ensure
  :hook
  (after-init . dimmer-mode)
  :custom
  (dimmer-fraction .5))

;;
;; Font
;; -----------------------------------------------------------------------------
;;
;; Grumpy wizards make toxic brew for the evil Queen and Jack.
;;
;; あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、
;; うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。

(use-package emacs
  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 128)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))

;;
;; Minibuffer
;; -----------------------------------------------------------------------------

;; https://github.com/abo-abo/swiper#ivy
(use-package ivy
  :ensure
  :bind (("C-c c-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :hook
  (after-init . ivy-mode)
  :custom
  (ivy-format-functions-alist '((t . ivy-format-function-arrow)))
  (ivy-re-builders-alist '((read-file-name-internal . ivy--regex-fuzzy)
                           (t . ivy--regex-plus))))

(use-package ivy-posframe
  :ensure
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  :hook
  (ivy-mode . ivy-posframe-mode))

(use-package ivy-rich
  :ensure
  :after ivy
  :hook
  (after-init . ivy-rich-mode))

;; https://github.com/abo-abo/swiper#counsel
(use-package counsel
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
  :custom
  (backup-inhibited t)
  (inhibit-startup-message t)
  :config
  (column-number-mode)
  (setq-default indent-tabs-mode nil))

;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure
  :hook
  (after-init . editorconfig-mode))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure
  :bind (("C-=" . er/expand-region)))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :ensure
  :hook
  (after-init . drag-stuff-global-mode)
  (after-init . drag-stuff-define-keys))

;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure
  :init (require 'smartparens-config)
  :hook
  (after-init . smartparens-global-mode))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Assistant
;; -----------------------------------------------------------------------------

;; https://github.com/justbur/emacs-which-key
(use-package which-key :ensure
  :hook
  (after-init . which-key-mode))

;;
;; Org Mode
;; -----------------------------------------------------------------------------

(use-package org
  :custom
  (org-directory "~/agenda")
  (org-agenda-files '("~/agenda")))

;;
;; History
;; -----------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :ensure
  :hook
  (after-init . global-undo-tree-mode))

;; https://github.com/m2ym/undohist-el
(use-package undohist
  :ensure
  :commands undohist-initialize
  :hook
  (after-init . undohist-initialize)
  :custom
  (undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

;;
;; Project
;; -----------------------------------------------------------------------------

;; https://projectile.readthedocs.io/en/latest/
(use-package projectile
  :ensure
  :hook
  (after-init . projectile-mode)
  :config
  (setq projectile-globally-ignored-directories (append '("elpa" "node_modules" "vendor")
                                                        projectile-globally-ignored-directories)))

(use-package neotree
  :ensure
  :bind (("<f8>" . neotree-toggle))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;
;; Autocomplete
;; -----------------------------------------------------------------------------

;; https://company-mode.github.io
(use-package company
  :ensure
  :hook
  (after-init . global-company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-idle-delay .3)
  (company-echo-delay 0)
  (company-begin-commands '(self-insert-command)))

(use-package company-box
  :ensure
  :hook
  (company-mode . company-box-mode))

(use-package company-quickhelp
  :ensure
  :hook
  (company-mode . company-quickhelp-mode))

;;
;; Syntax checker
;; -----------------------------------------------------------------------------

(use-package flycheck
  :ensure
  :hook
  (after-init . global-flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-highlighting-mode 'symbols)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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
  :if (display-graphic-p)
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
  :hook
  (lsp-mode . lsp-lens-mode)
  (go-mode . lsp-deferred)
  (java-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui :ensure :commands lsp-ui-mode)
(use-package company-lsp :ensure :commands company-lsp)
(use-package lsp-java :ensure :after lsp)

(use-package dap-mode
  :ensure
  :after lsp
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;
;; Language
;; -----------------------------------------------------------------------------

(use-package web-mode
  :ensure
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.vue\\'"
         "\\.[jt]sx?\\'")
  :init
  (defun web-mode-setup ()
    (setq web-mode-block-padding 0
          web-mode-script-padding 0
          web-mode-style-padding 0))
  (add-hook 'editorconfig-after-apply-functions (lambda (props) (web-mode-setup)))
  :hook
  (after-init . web-mode-setup))

(use-package prettier-js
  :ensure
  :hook (web-mode . prettier-js-mode))

(use-package yaml-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package gradle-mode :ensure)
(use-package groovy-mode :ensure)
(use-package kotlin-mode :ensure)
(use-package adoc-mode :ensure)
(use-package ssh-config-mode :ensure)

(use-package go-mode
  :ensure
  :init
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook
  (go-mode . lsp-go-install-save-hooks))

;;; init.el ends here
