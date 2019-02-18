;;; init.el -- My Emacs Configuration
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(when (file-exists-p (setq custom-file (locate-user-emacs-file "custom.el")))
  (load custom-file))

;;
;; Setup load-path
;; -----------------------------------------------------------------------------

(add-to-list 'load-path (locate-user-emacs-file "site-lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;;
;; Setup package
;; -----------------------------------------------------------------------------

(require 'package)

(defcustom package-contents-last-refreshed nil
  "Last refreshed time."
  :group 'package)

(add-to-list 'package-archives
	     '("melpha" . "https://melpa.org/packages/") t)
(package-initialize)

(let ((now (float-time)))
  (when (or (not package-contents-last-refreshed)
	    (> (- now package-contents-last-refreshed) (* 7 24 60 60)))
    (package-refresh-contents)
    (customize-save-variable 'package-contents-last-refreshed (float-time))))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;
;; Benchmark
;; -----------------------------------------------------------------------------

(use-package benchmark-init
  ;; https://github.com/dholm/benchmark-init-el
  :ensure t
  :hook ((after-init . benchmark-init/deactivate)))

;;
;; Environment variables
;; -----------------------------------------------------------------------------

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  :ensure t
  :if (member system-type '(gnu/linux ns))
  :init (exec-path-from-shell-initialize))

;;
;; Statistics
;; -----------------------------------------------------------------------------

(use-package keyfreq
  ;; https://github.com/dacap/keyfreq
  :ensure t
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;;
;; Input Method
;; -----------------------------------------------------------------------------

(use-package ddskk
  ;; http://openlab.jp/skk/
  :ensure t
  :defer t
  :bind (("C-x C-j" . skk-mode))
  :init (setq default-input-method "japanese-skk"))

;;
;; Dashboard
;; -----------------------------------------------------------------------------

(use-package dashboard
  ;; https://github.com/rakanalh/emacs-dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;;
;; Appearance
;; -----------------------------------------------------------------------------

(use-package nord-theme
  ;; https://emacsthemes.com/themes/nord-theme.html
  :ensure t
  :init (load-theme 'nord t))

(use-package smart-mode-line-powerline-theme
  ;; https://github.com/Malabarba/smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'powerline)
  (sml/setup))

(use-package mode-icons
  ;; http://projects.ryuslash.org/mode-icons/
  :ensure t
  :init (mode-icons-mode))

(use-package emojify
  ;; https://github.com/iqbalansari/emacs-emojify
  :ensure t
  :if window-system
  :init (global-emojify-mode))

;;
;; Font
;; ----------------------------------------------------------------------------------------

(use-package font
  ;; 567890123456789012345678901234567890123456789012345678901234567890123456789
  ;; Grumpy wizards make toxic brew for the evil Queen and Jack.
  ;;
  ;; あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、
  ;; うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。
  ;; 567890123456789012345678901234567890123456789012345678901234567890123456789
  :if window-system
  :bind (("C-+" . font/increase-height)
         ("C--" . font/decrease-height)
         ("C-0" . font/reset-height))
  :init
  (set-face-attribute 'default nil :family "Ricty Diminished")
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty Diminished"))
  (setq font/default-height 130)
  (font/reset-height))

;;
;; Minibuffer
;; -----------------------------------------------------------------------------

(use-package ivy
  ;; https://github.com/abo-abo/swiper#ivy
  :ensure t
  :defer t
  :delight
  :bind (("C-c c-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :init (ivy-mode)
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

(use-package counsel
  ;; https://github.com/abo-abo/swiper#counsel
  :ensure t
  :defer t
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

(use-package swiper
  ;; https://github.com/abo-abo/swiper#swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper)))

;;
;; Editor
;; -----------------------------------------------------------------------------

(use-package editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  :ensure t
  :delight
  :init (editorconfig-mode))

(use-package expand-region
  ;; https://github.com/magnars/expand-region.el
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package drag-stuff
  ;; https://github.com/rejeep/drag-stuff.el
  :ensure t
  :delight
  :init (drag-stuff-global-mode)
  :config (drag-stuff-define-keys))

(use-package smartparens
  ;; https://github.com/Fuco1/smartparens
  :ensure t
  :delight
  :init
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure t
  :defer t
  :disabled
  :hook prog-mode)

;;
;; History
;; -----------------------------------------------------------------------------

(use-package undo-tree
  ;; https://www.emacswiki.org/emacs/UndoTree
  :ensure t
  :delight
  :init (global-undo-tree-mode))

(use-package undohist
  ;; https://github.com/m2ym/undohist-el
  :ensure t
  :defer t
  :commands (undohist-initialize)
  :init (undohist-initialize)
  :config (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

;;
;; Project
;; -----------------------------------------------------------------------------

(use-package projectile
  ;; https://projectile.readthedocs.io/en/latest/
  :ensure t
  :delight
  :init (projectile-mode)
  :config
  (setq projectile-globally-ignored-directories
        (append '("elpa" "node_modules" "vendor")
                projectile-globally-ignored-directories)))

;;
;; Autocomplete
;; -----------------------------------------------------------------------------

(use-package company
  ;; https://company-mode.github.io
  :ensure t
  :delight
  :init (global-company-mode))

;;
;; Syntax checker
;; -----------------------------------------------------------------------------

(use-package flycheck
  ;; http://www.flycheck.org/en/lattest/
  :ensure t
  :init (global-flycheck-mode))

;;
;; Git
;; -----------------------------------------------------------------------------

(use-package magit
  ;; https://magit.vc
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package gitconfig-mode
  ;; https://github.com/magit/git-modes/blob/master/gitconfig-mode.el
  :ensure t
  :defer t)

(use-package gitignore-mode
  ;; https://github.com/magit/git-modes/blob/master/gitignore-mode.el
  :ensure t
  :defer t)

(use-package gitattributes-mode
  ;; https://github.com/magit/git-modes/blob/master/gitattributes-mode.el
  :ensure t
  :defer t)

;;
;; Docker
;; -----------------------------------------------------------------------------

(use-package docker
  ;; https://github.com/Silex/docker.el
  :ensure t
  :bind ("C-c d" . docker)
  :config
  (setq docker-image-run-arguments '("-i" "-t" "--rm")))

(use-package dockerfile-mode
  ;; https://github.com/spotify/dockerfile-mode
  :ensure t
  :defer t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;;
;; Markdown
;; -----------------------------------------------------------------------------

(use-package markdown-mode
  ;; https://jblevins.org/projects/markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md'" . markdown-mode)))

;;
;; Web
;; -----------------------------------------------------------------------------

(use-package web-mode
  ;; http://web-mode.org
  :ensure t
  :defer t
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

(use-package js2-mode
  ;; https://github.com/mooz/js2-mode
  :ensure t
  :defer t
  :mode (("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil)
  :init
  (use-package add-node-modules-path
    ;; https://github.com/codesuki/add-node-modules-path
    :ensure t
    :hook ((js2-mode . add-node-modules-path))))

;;
;; JSON
;; -----------------------------------------------------------------------------

(use-package json-mode
  ;; https://github.com/joshwnj/json-mode
  :ensure t
  :defer t)

;;
;; TypeScript
;; -----------------------------------------------------------------------------

(use-package typescript-mode
  ;; https://github.com/emacs-typescript/typescript.el
  :ensure t
  :defer t
  :init
  (use-package add-node-modules-path
    ;; https://github.com/codesuki/add-node-modules-path
    :ensure t
    :hook ((js2-mode . add-node-modules-path))))

(use-package tide
  ;; https://github.com/ananthakumaran/tide
  :ensure t
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; (use-package vue-mode
;;   ;; https://github.com/AdamNiederer/vue-mode
;;   :ensure t
;;   :defer t)

;;
;; YAML
;; -----------------------------------------------------------------------------

(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :defer t)

;; -----------------------------------------------------------------------------

(setq backup-inhibited t
      indent-tabs-mode nil
      inhibit-startup-message t)

(column-number-mode)

(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 40)))
(unless window-system
  (menu-bar-mode -1))

;;; init.el ends here
