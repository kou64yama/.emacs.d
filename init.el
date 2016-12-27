;;; init.el --- Emacs Configuration File
;;; Commentary:
;;; Code:

;; El-Get Basic Setup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-inhibited t t)
 '(column-number-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t :family "monofur for Powerline" :height 120))))

(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Yu Gothic"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; El-Get bundle
(el-get-bundle company-mode)
(el-get-bundle ddskk)
(el-get-bundle drag-stuff)
(el-get-bundle editorconfig)
(el-get-bundle elpa:counsel)
(el-get-bundle elpa:ivy)
(el-get-bundle emojify)
(el-get-bundle exec-path-from-shell)
(el-get-bundle expand-region)
(el-get-bundle flycheck)
(el-get-bundle git-modes)
(el-get-bundle js2-mode)
(el-get-bundle json-mode)
(el-get-bundle magit)
(el-get-bundle markdown-mode)
(el-get-bundle material-theme)
(el-get-bundle mode-icons)
(el-get-bundle multiple-cursors)
(el-get-bundle powerline)
(el-get-bundle rainbow-delimiters)
(el-get-bundle rainbow-mode)
(el-get-bundle smart-mode-line)
(el-get-bundle smartparens)
(el-get-bundle undo-tree)
(el-get-bundle undohist)
(el-get-bundle use-package)
(el-get-bundle volatile-highlights)
(el-get-bundle web-mode)
(el-get-bundle yascroll)
(el-get-bundle yasnippet)

;; Editor
(use-package editorconfig
  :init (add-hook 'after-init-hook #'editorconfig-mode))
(use-package expand-region
  :bind (("C-=" . er/expand-region)))
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))
(use-package drag-stuff
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode))
(use-package smartparens-config
  :init (add-hook 'after-init-hook #'smartparens-global-mode))
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook #'rainbow-mode))
(use-package volatile-highlights-mode
  :init (add-hook 'after-init-hook #'volatile-highlights-mode))

;; History
(use-package undo-tree
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))
(use-package undohist
  :commands (undohist-initialize)
  :init (undohist-initialize))

;; Input Method (日本語入力)
(use-package ddskk
  :init (setq default-input-method "japanese-skk"))

;; Appearance
(use-package material-theme
  :init (load-theme 'material t))
(use-package powerline)
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
        sml/theme 'powerline)
  (sml/setup))
(use-package yascroll
  :init (global-yascroll-bar-mode))
(use-package mode-icons
  :init (mode-icons-mode))
(use-package emojify
  :init (add-hook 'after-init-hook #'global-emojify-mode))

;; Minibuffer
(use-package ivy
  :bind (("C-s" . swiper)
  :init (add-hook 'after-init-hook #'ivy-mode)))
(use-package counsel
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
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

;; Autocomplete
(use-package company-mode
  :init (add-hook 'after-init-hook #'global-company-mode))

;; Syntax checker
(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

;; Snippet
(use-package yasnippet
  :init (add-hook 'after-init-hook #'yas-global-mode))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)))

;; Web
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.tag?\\'" . web-mode)))

;; JavaScript
(use-package js2-mode
  :mode (("\\.jsx?\\'" . js2-jsx-mode)))

;;; init.el ends here
