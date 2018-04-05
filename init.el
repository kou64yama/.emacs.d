;;; init.el -- Emacs configurations
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

;; Set `user-emacs-directory` if Emacs is launched with `emacs -q -l init.el`.
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Load `custom.el`.
(when (file-exists-p (setq custom-file (locate-user-emacs-file "custom.el")))
  (load custom-file))

;; Add "~/.emacs.d/site-lisp" and "~/.emacs.d/lisp" to `load-path`.
(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'sensible)
(require 'eplug)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eplug 'adoc-mode)
(eplug 'company)
(eplug 'company-flow)
(eplug 'company-go)
(eplug 'counsel)
(eplug 'counsel-projectile)
(eplug 'ddskk)
(eplug 'drag-stuff)
(eplug 'editorconfig)
(eplug 'emojify)
(eplug 'exec-path-from-shell)
(eplug 'expand-region)
(eplug 'flycheck)
(eplug 'flycheck-flow)
(eplug 'font-utils)
(eplug 'git-gutter-fringe+)
(eplug 'go-mode)
(eplug 'google-translate)
(eplug 'groovy-mode)
(eplug 'ivy)
(eplug 'js2-mode)
(eplug 'json-mode)
(eplug 'keyfreq)
(eplug 'magit)
(eplug 'markdown-mode)
(eplug 'material-theme)
(eplug 'mode-icons)
(eplug 'multiple-cursors)
(eplug 'projectile)
(eplug 'rainbow-delimiters)
(eplug 'smart-mode-line-powerline-theme)
(eplug 'smartparens)
(eplug 'swiper)
(eplug 'tide)
(eplug 'typescript-mode)
(eplug 'undo-tree)
(eplug 'undohist)
(eplug 'use-package)
(eplug 'web-mode)
(eplug 'yascroll)
(eplug 'yasnippet-snippets)

(and (eplug-check)
     (y-or-n-p (format "Install %s? " (eplug-check)))
     (eplug-install))

;;
;; Window
;; -----------------------------------------------------------------------------

(when window-system
  (add-to-list 'default-frame-alist '(alpha . .95))
  (add-to-list 'default-frame-alist '(width . 100))
  (add-to-list 'default-frame-alist '(height . 30)))

;;
;; Benchmark
;; -----------------------------------------------------------------------------

;; keyfreq
;; https://github.com/dacap/keyfreq
(use-package keyfreq
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;;
;; Environment Variables
;; -----------------------------------------------------------------------------

;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;;
;; Editor
;; -----------------------------------------------------------------------------

;; EditorConfig
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :init (editorconfig-mode))

;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :init (drag-stuff-global-mode)
  :config (drag-stuff-define-keys))

;; Smartparens
;; https://github.com/Fuco1/smartparens
(use-package smartparens-config
  :init (smartparens-global-mode))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;
;; Minibuffer
;; -----------------------------------------------------------------------------

;; Ivy
;; https://github.com/abo-abo/swiper#ivy
(use-package ivy
  :bind (("C-c c-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :init (ivy-mode))

;; Counsel
;; https://github.com/abo-abo/swiper#counsel
(use-package counsel
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
         :map read-expression-map
         ("C-r" . counsel-expression-history)))

;; Swiper
;; https://github.com/abo-abo/swiper#swiper
(use-package swiper
  :bind (("C-s" . swiper)))

;;
;; Appearance
;; -----------------------------------------------------------------------------

;; Material Theme
;; https://emacsthemes.com/themes/material-theme.html
(use-package material-theme
  :init (load-theme 'material t))

;; Smart-mode-line
;; https://github.com/Malabarba/smart-mode-line/
(use-package smart-mode-line
  :init
  (setq sml/theme 'powerline)
  (sml/setup))

;; yascroll.el
;; https://github.com/m2ym/yascroll-el
(use-package yascroll
  :if window-system
  :init (global-yascroll-bar-mode))

;; font-utils
;; https://github.com/rolandwalker/font-utils
(use-package font-utils
  :if window-system
  :init
  (let ((family (font-utils-first-existing-font
            '("saxMono"
              "monofur for Powerline"
              "Consolas"))))
    (when (set-face-attribute 'default nil :family family :height 120))))

;; Mode icons
;; http://projects.ryuslash.org/mode-icons/
(use-package mode-icons
  :init (mode-icons-mode))

;; Emojify
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :init (global-emojify-mode))

;;
;; History
;; -----------------------------------------------------------------------------

;; Undo Tree
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :init (global-undo-tree-mode))

;; undohist
;; https://github.com/m2ym/undohist-el
(use-package undohist
  :commands (undohist-initialize)
  :init (undohist-initialize)
  :config
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;;
;; Input Method
;; -----------------------------------------------------------------------------

;; Daredevil SKK
;; http://openlab.jp/skk/
(use-package ddskk
  :bind (("C-x C-j" . skk-mode))
  :init (setq default-input-method "japanese-skk"))

;;
;; Project
;; -----------------------------------------------------------------------------

;; Projectile
;; https://projectile.readthedocs.io/en/latest/
(use-package projectile
  :init (projectile-mode)
  :config
  (setq projectile-globally-ignored-directories
        (append '("elpa" "node_modules" "vendor")
                projectile-globally-ignored-directories)))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
;; (use-package counsel-projectile
;;   :init (add-hook 'projectile-mode-hook 'counsel-projectile-mode))
(use-package counsel-projectile
  :init
  (setq counsel-projectile-mode t))

;;
;; CVS
;; -----------------------------------------------------------------------------

;; Magit
;; https://magit.vc
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; git-gutter-fringe+
;; https://github.com/nonsequitur/git-gutter-fringe-plus
(use-package git-gutter-fringe+
  :if window-system
  :init (global-git-gutter+-mode)
  :config
  (fringe-helper-define 'git-gutter-fr+-added '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr+-deleted '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr+-modified '(top repeat) "xxxx...."))

;;
;; Autocomplete
;; -----------------------------------------------------------------------------

;; company
;; https://company-mode.github.io
(use-package company
  :init (global-company-mode))

;; Company flow
;; https://github.com/aaronjensen/company-flow
(use-package company-flow
  :init
  (defun company-flow-setup ()
    (add-to-list 'company-backends 'company-flow))
  (add-hook 'company-mode-hook 'company-flow-setup))

;; Company Go
;; https://github.com/nsf/gocode/blob/master/emacs-company
(use-package company-go
  :init
  (defun company-go-setup ()
    (add-to-list 'company-backends 'company-go))
  (add-hook 'company-mode-hook 'company-go-setup)
  (setq gofmt-command (executable-find "goimports")))

;;
;; Syntax checker
;; -----------------------------------------------------------------------------

;; Flycheck
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe))

;; flycheck-flow
;; https://github.com/lbolla/emacs-flycheck-flow
(use-package flycheck-flow
  :init
  (defun setup-flycheck-flow ()
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)
    (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage))
  (add-hook 'flycheck-mode-hook 'setup-flycheck-flow))

;;
;; Translate
;; -----------------------------------------------------------------------------

(use-package google-translate-default-ui
  :bind (("C-c t" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist
        '(("en" . "ja") ("ja" . "en"))))

;;
;; Snippet
;; -----------------------------------------------------------------------------

;; Yet another snippet extension
;; https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :init (yas-global-mode))

;;
;; Org
;; -----------------------------------------------------------------------------

(use-package org
  :config
  (setq org-agenda-files '("~/Agenda")))

;;
;; HTML
;; -----------------------------------------------------------------------------

;; web-mode.el
;; http://web-mode.org
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode)))

;;
;; JavaScript
;; -----------------------------------------------------------------------------

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil))

;;
;; TypeScript
;; -----------------------------------------------------------------------------

;; Tide
;; https://github.com/ananthakumaran/tide
(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'before-save-hook 'tide-format-before-save))

;;
;; Go
;; -----------------------------------------------------------------------------

;; go-mode
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

;;; init.el ends here
