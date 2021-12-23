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

;; https://github.com/conao3/leaf.el#install
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

;;
;; Statistics
;; ---------------------------------------------------------------------

;; https://github.com/dholm/benchmark-init-el
(leaf benchmark-init
  :ensure t
  :hook (after-init-hook . benchmark-init/deactivate)
  :init (benchmark-init/activate))

;; https://github.com/dacap/keyfreq
(leaf keyfreq
  :ensure t
  :init
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;;
;; Environment Variables
;; ---------------------------------------------------------------------

;; https://github.com/purcell/exec-path-from-shell
(leaf exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;;
;; Startup
;; ---------------------------------------------------------------------

(leaf cus-start
  :custom
  (inhibit-startup-message . t))

(leaf server
  :disabled t
  :commands server-running-p
  :init
  (unless (server-running-p)
    (server-start)))

;; https://github.com/emacs-dashboard/emacs-dashboard
(leaf dashboard
  :ensure t
  :init (dashboard-setup-startup-hook)
  :setq ((dashboard-items . '((projects . 15)
                              (recents . 15)))
         (dashboard-set-heading-icons . t)
         (dashboard-set-file-icons . t)))

;;
;; Appearance
;; ---------------------------------------------------------------------

(leaf cus-start
  :custom
  (menu-bar-mode . t)
  (tool-bar-mode . nil)
  (scroll-bar-mode . nil)
  :init
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(width . 100))
  (add-to-list 'default-frame-alist '(height . 36)))

;; https://github.com/srcery-colors/srcery-emacs
(leaf srcery-theme
  :ensure t
  :init (load-theme 'srcery t))

;; https://github.com/seagle0128/doom-modeline
(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode))

;; https://github.com/iqbalansari/emacs-emojify
(leaf emojify
  :ensure t
  :setq ((emojify-display-style . 'unicode))
  :init (global-emojify-mode))

;;
;; Font
;; ---------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------
;; Grumpy wizards make toxic brew for the evil Queen and Jack.-----------
;; あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、--
;; うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。--------
;; 😃😇😍😜😸🙈🐺🐰👽🐉💰🏡🎅🍪🍕🚀🚻💩📷📦------------------------------
;; ----------------------------------------------------------------------

(leaf cus-start
  :if (display-graphic-p)
  :init
  (set-face-attribute 'default nil :family "Anka/Coder" :height 120)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Ricty Diminished" :size 14))
  (set-fontset-font (frame-parameter nil 'font)
                    '(#x1F000 . #x1FAFF)
                    (font-spec :family "Noto Emoji" :size 11)))

;;
;; Tab
;; ---------------------------------------------------------------------

;; https://github.com/ema2159/centaur-tabs
(leaf centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :setq ((centaur-tabs-style . "bar")
         (centaur-tabs-set-icons . t)
         (centaur-tabs-set-bar . 'left)
         (centaur-tabs-set-close-button . nil)
         (centaur-tabs-cycle-scope . 'tabs)))

;;
;; Minibuffer
;; ---------------------------------------------------------------------

;; https://github.com/raxod502/prescient.el
(leaf prescient
  :ensure t
  :commands prescient-persist-mode
  :init (prescient-persist-mode))

;; https://github.com/raxod502/selectrum
(leaf selectrum
  :ensure t
  :init (selectrum-mode))

;; https://github.com/raxod502/selectrum
(leaf selectrum-prescient
  :ensure t
  :after prescient selectrum
  :init (selectrum-prescient-mode))

;;
;; Input Method
;; ---------------------------------------------------------------------

;; https://github.com/skk-dev/ddskk
(leaf ddskk
  :ensure t
  :bind
  ("C-x C-j" . skk-mode)
  :setq
  (default-input-method . "japanese-skk"))

;;
;; Assistant
;; ---------------------------------------------------------------------

;; https://github.com/justbur/emacs-which-key
(leaf which-key
  :ensure t
  :init (which-key-mode))

;;
;; Editor
;; ---------------------------------------------------------------------

(leaf cus-start
  :custom
  (indent-tabs-mode . nil)
  (backup-inhibited . t)
  :setq
  (recentf-exclude . '("/\\.emacs\\d/local/"))
  :setq-default `((indicate-empty-lines . t)
                  (indicate-buffer-boundaries . '((top . nil) (bottom . right) (down . right))))
  :init
  (global-hl-line-mode)
  (global-display-line-numbers-mode))

;; https://github.com/aspiers/smooth-scrolling
(leaf smooth-scrolling
  :ensure t
  :init (smooth-scrolling-mode))

;; https://github.com/editorconfig/editorconfig-emacs
(leaf editorconfig
  :ensure t
  :init (editorconfig-mode))

;; https://github.com/magnars/expand-region.el
(leaf expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; https://github.com/magnars/multiple-cursors.el
(leaf multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

;; https://github.com/rejeep/drag-stuff.el
(leaf drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode)
  (drag-stuff-define-keys))

;; https://github.com/Fuco1/smartparens
(leaf smartparens
  :ensure t
  :require smartparens-config
  :init (smartparens-global-mode))

;; https://github.com/Fanael/rainbow-delimiters
(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;
;; Search
;; ---------------------------------------------------------------------

;; https://github.com/raxod502/ctrlf
(leaf ctrlf
  :ensure t
  :init (ctrlf-mode))

;;
;; History
;; ---------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/UndoTree
(leaf undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; https://github.com/emacsorphanage/undohist
(leaf undohist
  :ensure t
  ;; https://github.com/emacsorphanage/undohist/issues/6#issuecomment-602962791
  ;; > this package would not further develop
  :disabled t
  :commands undohist-initialize
  :init (undohist-initialize)
  :custom
  (undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

;;
;; Project
;; ---------------------------------------------------------------------

;; https://github.com/bbatsov/projectile
(leaf projectile
  :ensure t
  :init (projectile-mode)
  :config
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "vendor"))

;;
;; VCS
;; ---------------------------------------------------------------------

;; https://github.com/magit/magit
(leaf magit :ensure t)

;; https://github.com/emacsorphanage/git-gutter-fringe
(leaf git-gutter-fringe
  :ensure t
  :commands fringe-helper-define
  :init
  (global-git-gutter-mode)
  (fringe-helper-define 'git-gutter-fr:added '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:deleted '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:modified '(top repeat) "xxxx...."))

;;
;; Autocomplete
;; ---------------------------------------------------------------------

;; https://company-mode.github.io
(leaf company
  :ensure t
  :init (global-company-mode)
  :custom
  (company-tooltip-limit . 20)
  (company-idle-delay . .3)
  (company-begin-commands . '(self-insert-command)))

;; https://github.com/sebastiencs/company-box
(leaf company-box
  :ensure t
  :after company
  :hook (company-mode-hook . company-box-mode))

;; https://github.com/raxod502/prescient.el
(leaf company-prescient
  :ensure t
  :after prescient company
  :init (company-prescient-mode))

;;
;; Docker
;; ---------------------------------------------------------------------

;; https://github.com/Silex/docker.el
(leaf docker
  :ensure t
  :bind
  ("C-c d" . docker))

;;
;; LSP
;; ---------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode
(leaf lsp-mode
  :ensure t
  :hook (lsp-mode-hook . lsp-lens-mode))

;; https://github.com/emacs-lsp/lsp-ui
(leaf lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode))

;;
;; Syntax checker
;; ---------------------------------------------------------------------

;; https://www.flycheck.org/en/latest/
(leaf flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-indication-mode . 'right-fringe))

;;
;; Language
;; ---------------------------------------------------------------------

(leaf ssh-config-mode :ensure t)

;; ShellScript
(leaf sh-mode
  :hook (sh-mode-hook . lsp))

(leaf shfmt
  :ensure t
  :hook (sh-mode-hook . shfmt-on-save-mode))

;; HTML, JavaScript, CSS, ...
(leaf web-mode
  :ensure t
  :mode
  "\\.phtml\\'"
  "\\.tpl\\.php'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.html?\\'"
  "\\.vue\\'"
  "\\.[jt]sx?\\'"
  :config
  (defun web-mode-setup ()
    (setq web-mode-block-padding 0
          web-mode-script-padding 0
          web-mode-style-padding 0))
  (add-hook 'editorconfig-after-apply-functions (lambda (props) (web-mode-setup)))
  :hook (web-mode-hook . lsp))

(leaf prettier-js
  :ensure t
  :hook (web-mode-hook . prettier-js-mode))

(leaf dockerfile-mode
  :ensure t
  :hook (dockerfile-mode-hook . lsp))

(leaf kotlin-mode
  :ensure t
  :hook (kotlin-mode-hook . lsp))

(leaf go-mode
  :ensure t
  :hook (go-mode-hook . lsp))

(leaf yaml-mode
  :ensure t
  :hook (yaml-mode-hook . lsp))

(leaf gradle-mode :ensure t)
(leaf groovy-mode :ensure t)
(leaf adoc-mode :ensure t)

;;; init.el ends here
