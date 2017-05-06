;;; init.el --- Emacs Configuration
;;; Commentary:

;; Copyright © 2016-present YAMADA Koji. All rights reserved.

;; This source code is licensed under the MIT license found in the
;; LISENCE file in the root directory of this source tree.

;;; Code:

;; Set `user-emacs-directory' if Emacs is launched with `emacs -q -l`.
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Load custom file.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Install `use-package' if it isn't installed.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

;; Define `package-bundle' and `package-bundle-sync'.
(defvar package-bundled-packages nil)

(defun package-bundle (pkg)
  "Add PKG to package-bundled-packages."
  (add-to-list 'package-bundled-packages pkg t))

(defun package-bundle-sync ()
  "Sync bundled packages."
  (custom-set-variables
   '(package-bundleed-packages package-bundled-packages))
  (package-install-selected-packages))

;; Bundle packages.
(package-bundle 'all-the-icons-dired)
(package-bundle 'company)
(package-bundle 'counsel)
(package-bundle 'ddskk)
(package-bundle 'drag-stuff)
(package-bundle 'editorconfig)
(package-bundle 'emojify)
(package-bundle 'exec-path-from-shell)
(package-bundle 'expand-region)
(package-bundle 'flycheck)
(package-bundle 'ivy)
(package-bundle 'js2-mode)
(package-bundle 'magit)
(package-bundle 'markdown-mode)
(package-bundle 'material-theme)
(package-bundle 'mode-icons)
(package-bundle 'multiple-cursors)
(package-bundle 'rainbow-delimiters)
(package-bundle 'rainbow-mode)
(package-bundle 'smart-mode-line-powerline-theme)
(package-bundle 'smartparens)
(package-bundle 'swiper)
(package-bundle 'undo-tree)
(package-bundle 'undohist)
(package-bundle 'use-package)
(package-bundle 'volatile-highlights)
(package-bundle 'web-mode)
(package-bundle 'yascroll)
(package-bundle 'yasnippet)

(package-bundle-sync)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

(custom-set-faces
 '(default ((t :family "monofur for Powerline" :height 120))))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Yu Gothic"))

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
  :init (add-hook 'after-init-hook #'editorconfig-mode))

;; expand-region.el
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; multiple-cursors.el
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; Drag Stuff
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode))

;; Smartparens
;; https://github.com/Fuco1/smartparens
(use-package smartparens-config
  :init (add-hook 'after-init-hook #'smartparens-global-mode))

;; rainbow-delimiters
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; rainbow-mode
;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

;; Volatile Highlights
;; https://www.emacswiki.org/emacs/VolatileHighlights
(use-package volatile-highlights
  :init (add-hook 'after-init-hook #'volatile-highlights-mode))

;;
;; History
;; -----------------------------------------------------------------------------

;; Undo Tree
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))

;; undohist
;; https://github.com/m2ym/undohist-el
(use-package undohist
  :commands (undohist-initialize)
  :init (undohist-initialize)
  :config
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

;;
;; Input Method
;; -----------------------------------------------------------------------------

;; Daredevil SKK
;; http://openlab.jp/skk/
(use-package ddskk
  :bind (("C-x C-j" . skk-mode))
  :init (setq default-input-method "japanese-skk"))

;;
;; Appearance
;; -----------------------------------------------------------------------------

;; Material Theme
;; https://emacsthemes.com/themes/material-theme.html
(use-package material-theme
  :init (load-theme 'material))

;; Smart-mode-line
;; https://github.com/Malabarba/smart-mode-line/
(use-package smart-mode-line-powerline-theme
  :init
  (setq sml/theme 'powerline)
  (sml/setup))

;; yascroll.el
;; https://github.com/m2ym/yascroll-el
(use-package yascroll
  :init (global-yascroll-bar-mode 1))

;; Mode icons
;; http://projects.ryuslash.org/mode-icons/
(use-package mode-icons
  :init (add-hook 'after-init-hook #'mode-icons-mode))

;; Emojify
;; https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :init (add-hook 'after-init-hook #'global-emojify-mode))

;; all-the-icons-dired
;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;;
;; Minibuffer
;; -----------------------------------------------------------------------------

;; Ivy
;; https://github.com/abo-abo/swiper#ivy
(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :init (ivy-mode 1))

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
;; Autocomplete
;; -----------------------------------------------------------------------------

;; company-mode
;; https://company-mode.github.io
(use-package company-mode
  :init (add-hook 'after-init-hook #'global-company-mode))

;;
;; Syntax checker
;; -----------------------------------------------------------------------------

;; Flycheck
;; http://www.flycheck.org/en/latest/
(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

;;
;; Snippet
;; -----------------------------------------------------------------------------

;; Yet another snippet extension
;; https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :init (add-hook 'after-init-hook #'yas-global-mode))

;;
;; Markdown
;; -----------------------------------------------------------------------------

;; Emacs Markdown Mode
;; http://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md\\$" . markdown-mode)
         ("\\.markdown\\$" . markdown-mode)))

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

;;; init.el ends here
