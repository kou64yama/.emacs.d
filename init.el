;;; init.el -- Emacs Configuration File.

;;; Commentary:

;; Copyright (c) 2016-present Yamada Koji.
;;
;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(setq custom-file (locate-user-emacs-file "init-local.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; https://github.com/conao3/leaf.el#install
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (and (package-installed-p 'leaf) (package-installed-p 'benchmark-init))
    (package-refresh-contents)
    (package-install 'leaf)
    (package-install 'benchmark-init))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

;; https://github.com/dholm/benchmark-init-el
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Environment variables

(leaf exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :setq
  (exec-path-from-shell-arguments . nil)
  :init
  (exec-path-from-shell-initialize))

(leaf add-node-modules-path
  ;; https://github.com/codesuki/add-node-modules-path
  :ensure t
  :hook
  (prog-mode-hook . add-node-modules-path))

;; Statistics

(leaf keyfreq
  ;; https://github.com/dacap/keyfreq
  :ensure t
  :global-minor-mode keyfreq-mode keyfreq-autosave-mode)

;; Editor

(leaf cus-start
  :custom
  (backup-inhibited . t)
  (menu-bar-mode . nil)
  (tool-bar-mode . nil)
  (scroll-bar-mode . nil)
  (line-number-mode . t)
  (column-number-mode . t)
  (indent-tabs-mode . nil)
  :config
  (dolist (alist '((ns-transparent-titlebar . t)
                   (ns-appearance . dark)
                   (width . 120)
                   (height . 42)
                   (alpha . 0.85)))
    (add-to-list 'default-frame-alist alist t)))

(leaf recentf
  :custom
  (recentf-exclude . '((expand-file-name "~/\\.emacs\\.d/elpa/"))))

(leaf hl-line
  :global-minor-mode global-hl-line-mode)

(leaf display-line-numbers
  :hook
  (prog-mode-hook . display-line-numbers-mode))

(leaf autorevert
  :global-minor-mode global-auto-revert-mode)

(leaf editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  :ensure t
  :config
  (editorconfig-mode 1))

(leaf expand-region
  ;; https://github.com/magnars/expand-region.el
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(leaf multiple-cursors
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(leaf drag-stuff
  ;; https://github.com/rejeep/drag-stuff.el
  :ensure t
  :global-minor-mode drag-stuff-global-mode
  :config
  (drag-stuff-define-keys))

(leaf smartparens
  ;; https://github.com/Fuco1/smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode)

(leaf rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf font
  ;; Grumpy wizards make toxic brew for the evil Queen and Jack.----------------
  ;; あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、-------
  ;; うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。-------------
  ;; 😃😇😍😜😸🙈🐺🐰👽🐉💰🏡🎅🍪🍕🚀🚻💩📷📦-----------------------------------
  :if (display-graphic-p)
  :init
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 120)
  (set-fontset-font nil
                    'japanese-jisx0208
                    (font-spec :family "Ricty Diminished" :size 14))
  (dolist (symbol-subgroup
           '((#x0250 . #x02AF)   ;; IPA Extensions
             (#x0370 . #x03FF)   ;; Greek and Coptic
             (#x0500 . #x052F)   ;; Cyrillic Supplement
             (#x2000 . #x206F)   ;; General Punctuation
             (#x2070 . #x209F)   ;; Superscripts and Subscripts
             (#x20A0 . #x20CF)   ;; Currency Symbols
             (#x2100 . #x214F)   ;; Letterlike Symbols
             (#x2150 . #x218F)   ;; Number Forms
             (#x2190 . #x21FF)   ;; Arrows
             (#x2200 . #x22FF)   ;; Mathematical Operators
             (#x2300 . #x23FF)   ;; Miscellaneous Technical
             (#x2400 . #x243F)   ;; Control Pictures
             (#x2440 . #x245F)   ;; Optical Char Recognition
             (#x2460 . #x24FF)   ;; Enclosed Alphanumerics
             (#x25A0 . #x25FF)   ;; Geometric Shapes
             (#x2600 . #x26FF)   ;; Miscellaneous Symbols
             (#x2700 . #x27bF)   ;; Dingbats
             (#x27C0 . #x27EF)   ;; Misc Mathematical Symbols-A
             (#x27F0 . #x27FF)   ;; Supplemental Arrows-A
             (#x2900 . #x297F)   ;; Supplemental Arrows-B
             (#x2980 . #x29FF)   ;; Misc Mathematical Symbols-B
             (#x2A00 . #x2AFF)   ;; Suppl. Math Operators
             (#x2B00 . #x2BFF)   ;; Misc Symbols and Arrows
             (#x2E00 . #x2E7F)   ;; Supplemental Punctuation
             (#x4DC0 . #x4DFF)   ;; Yijing Hexagram Symbols
             (#xFE10 . #xFE1F)   ;; Vertical Forms
             (#x10100 . #x1013F) ;; Aegean Numbers
             (#x102E0 . #x102FF) ;; Coptic Epact Numbers
             (#x1D000 . #x1D0FF) ;; Byzanthine Musical Symbols
             (#x1D200 . #x1D24F) ;; Ancient Greek Musical Notation
             (#x1F0A0 . #x1F0FF) ;; Playing Cards
             (#x1F100 . #x1F1FF) ;; Enclosed Alphanumeric Suppl
             (#x1F300 . #x1F5FF) ;; Misc Symbols and Pictographs
             (#x1F600 . #x1F64F) ;; Emoticons
             (#x1F650 . #x1F67F) ;; Ornamental Dingbats
             (#x1F680 . #x1F6FF) ;; Transport and Map Symbols
             (#x1F700 . #x1F77F) ;; Alchemical Symbols
             (#x1F780 . #x1F7FF) ;; Geometric Shapes Extended
             (#x1F800 . #x1F8FF))) ;; Supplemental Arrows-C
    (set-fontset-font nil
                      symbol-subgroup
                      (font-spec :family "Noto Emoji" :size 11))))

(leaf emojify
  ;; https://github.com/iqbalansari/emacs-emojify
  :ensure t
  :global-minor-mode global-emojify-mode
  :custom
  (emojify-display-style . 'unicode))

(leaf smooth-scrolling
  ;; https://github.com/aspiers/smooth-scrolling
  :ensure t
  :global-minor-mode smooth-scrolling-mode)

;; Minibuffer

(leaf ido
  :global-minor-mode ido-mode
  :custom
  (ido-enable-flex-matching . t)
  (ido-use-faces . nil)
  :config
  (ido-everywhere 1))

(leaf flx-ido
  ;; https://github.com/lewang/flx
  :ensure t
  :global-minor-mode flx-ido-mode)

(leaf ido-completing-read+
  ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
  :ensure t
  :global-minor-mode ido-ubiquitous-mode)

(leaf prescient
  ;; https://github.com/raxod502/prescient.el
  :ensure t
  :disabled t
  :commands prescient-persist-mode
  :global-minor-mode prescient-persist-mode)

(leaf selectrum
  ;; https://github.com/raxod502/selectrum
  :ensure t
  :disabled t
  :global-minor-mode selectrum-mode)

(leaf selectrum-prescient
  ;; https://github.com/raxod502/selectrum
  :ensure t
  :disabled t
  :after prescient selectrum
  :global-minor-mode selectrum-prescient-mode)

(leaf which-key
  ;; https://github.com/justbur/emacs-which-key
  :ensure t
  :global-minor-mode which-key-mode)

(leaf ddskk
  ;; https://github.com/skk-dev/ddskk
  :ensure t
  :bind
  ("C-x C-j" . skk-mode)
  :setq-default
  (default-input-method . "japanese-skk"))

;; Tree

(leaf treemacs
  ;; https://github.com/Alexander-Miller/treemacs
  :ensure t
  :bind
  ("M-0" . treemacs-select-window)
  ("C-x t 1" . treemacs-delete-other-windows)
  ("C-x t t" . treemacs)
  ("C-x t d" . treemacs-select-directory)
  ("C-x t B" . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(leaf treemacs-all-the-icons
  :ensure t
  :after treemacs all-the-icons)

(leaf treemacs-projectile
  :ensure t
  :after treemacs projectile)

(leaf treemacs-magit
  :ensure t
  :after treemacs magit)

;; Appearance

(leaf modus-themes
  ;; https://gitlab.com/protesilaos/modus-themes
  :ensure t
  :bind ("<f5>" . modus-themes-toggle)
  :setq
  (modus-themes-italic-constructs . t)
  (modus-themes-bold-constructs . t)
  (modus-themes-subtle-line-numbers . t)
  (modus-themes-mode-line . '(moody borderless (height . 0.9)))
  (modus-themes-hl-line . '(intense))
  (modus-themes-region . '(bg-only))
  :init
  (modus-themes-load-themes)
  :config
  (load-theme 'modus-vivendi))

(leaf moody
  ;; https://github.com/tarsius/moody
  :ensure t
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(leaf minions
  ;; https://github.com/tarsius/minions
  :ensure t
  :config
  (minions-mode))

(leaf all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  :ensure t
  :if (display-graphic-p))

(leaf all-the-icons-dired
  ;; https://github.com/jtbm37/all-the-icons-dired
  :ensure t
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

;; Dashboard

(leaf dashboard
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items . '((projects . 15) (recents . 15)))
  (dashboard-set-heading-icons . t)
  (dashboard-set-file-icons . t))

;; Project

(leaf projectile
  ;; https://github.com/bbatsov/projectile
  :ensure t
  :global-minor-mode projectile-mode
  :bind
  (:projectile-mode-map
   ("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)))

;; Search

(leaf ctrlf
  ;; https://github.com/raxod502/ctrlf
  :ensure t
  :global-minor-mode ctrlf-mode)

;; History

(leaf undo-tree
  ;; https://www.emacswiki.org/emacs/UndoTree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :custom
  (undo-tree-auto-save-history . nil))

;; SCM

(leaf magit
  ;; https://magit.vc
  :ensure t
  :custom
  (magit-repository-directories . '(("~/ghq" . 3))))

(leaf forge
  ;; https://magit.vc/manual/forge/
  :ensure t
  :after magit)

(leaf git-gutter-fringe
  ;; https://github.com/emacsorphanage/git-gutter-fringe
  :ensure t
  :commands fringe-helper-define
  :global-minor-mode global-git-gutter-mode
  :init
  (fringe-helper-define 'git-gutter-fr:added '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:deleted '(top repeat) "xxxx....")
  (fringe-helper-define 'git-gutter-fr:modified '(top repeat) "xxxx...."))

;; Syntax checker

(leaf flycheck
  ;; https://www.flycheck.org/en/latest/
  :ensure t
  :global-minor-mode global-flycheck-mode
  :custom
  (flycheck-highlighting-mode . 'symbols))

;; Completion

(leaf company
  ;; http://company-mode.github.io
  :ensure t
  :global-minor-mode global-company-mode)

(leaf company-box
  ;; https://github.com/sebastiencs/company-box
  :ensure t
  :after company
  :hook
  (company-mode-hook . company-box-mode))

(leaf company-prescient
  ;; https://github.com/raxod502/prescient.el
  :ensure t
  :disabled t
  :after company prescient
  :global-minor-mode company-prescient-mode)

;; Snippet

(leaf yasnippet
  ;; https://github.com/joaotavora/yasnippet
  :ensure t
  :global-minor-mode yas-global-mode)

(leaf yasnippet-snippets
  ;; https://github.com/AndreaCrotti/yasnippet-snippets
  :ensure t
  :after yasnippet)

;; LSP

(leaf lsp-mode
  ;; https://emacs-lsp.github.io/lsp-mode/
  :ensure t)

(leaf lsp-ui
  ;; https://emacs-lsp.github.io/lsp-ui/
  :ensure t
  :custom
  (lsp-ui-sideline-show-diagnostics . t)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-sideline-show-code-actions . t)
  (lsp-ui-sideline-update-mode . 'line)
  (lsp-ui-imenu-auto-refresh . t)
  (lsp-ui-imenu-refresh-delay . 1))

(leaf lsp-treemacs
  ;; https://github.com/emacs-lsp/lsp-treemacs
  :ensure t
  :global-minor-mode lsp-treemacs-sync-mode)

(leaf dap-mode
  ;; https://emacs-lsp.github.io/dap-mode/
  :ensure t
  :after lsp-mode
  :global-minor-mode dap-auto-configure-mode)

;; Highlighting

(leaf tree-sitter-langs
  ;; https://emacs-tree-sitter.github.io
  :ensure t
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

;; Formatter

(leaf prettier-js
  ;; https://github.com/prettier/prettier-emacs
  :ensure t)

;; Docker

(leaf docker
  ;; https://github.com/Silex/docker.el
  :ensure t
  :bind
  ("C-c d" . docker))

(leaf dockerfile-mode
  ;; https://github.com/spotify/dockerfile-mode
  :ensure t
  :hook
  (dockerfile-mode-hook . lsp))

(leaf docker-tramp
  ;; https://github.com/emacs-pe/docker-tramp.el
  :ensure t)

;; Kubernetes

(leaf kubernetes
  ;; https://github.com/kubernetes-el/kubernetes-el
  :ensure t)

;; Markdown

(leaf markdown-mode
  ;; https://jblevins.org/projects/markdown-mode/
  :ensure t
  :hook
  (markdown-mode-hook . lsp)
  (markdown-mode-hook . prettier-js-mode))

;; AsciiDoc

(leaf adoc-mode
  ;; https://github.com/sensorflo/adoc-mode
  :ensure t
  :mode
  "\\.adoc\\'")

;; ShellScript

(leaf sh-mode
  :hook
  (sh-mode-hook . lsp)
  (sh-mode-hook . tree-sitter-mode))

(leaf shfmt
  ;; https://github.com/purcell/emacs-shfmt
  :ensure t
  :hook (sh-mode-hook . shfmt-on-save-mode))

;; Web

(leaf web-mode
  ;; https://web-mode.org
  :ensure t
  :mode
  "\\.vue\\'"
  :hook
  (web-mode-hook . lsp)
  :config
  (defun web-mode-setup ()
    (setq web-mode-block-padding 0
          web-mode-script-padding 0
          web-mode-sttyle-padding 0))
  (add-hook 'editorconfig-after-apply-functions (lambda (props) (web-mode-setup))))

(leaf js-mode
  :mode "\\.jsx?\\'"
  :hook
  (js-mode-hook . lsp)
  (js-mode-hook . tree-sitter-mode)
  (js-mode-hook . prettier-js-mode))

(leaf typescript-mode
  :ensure t
  :mode "\\.tsx?\\'"
  :hook
  (typescript-mode-hook . lsp)
  (typescript-mode-hook . tree-sitter-mode)
  (typescript-mode-hook . prettier-js-mode))

(leaf css-mode
  :hook
  (css-mode-hook . lsp)
  (css-mode-hook . tree-sitter-mode)
  (css-mode-hook . prettier-js-mode))

;; JSON

(leaf json-mode
  ;; https://github.com/joshwnj/json-mode
  :ensure t
  :hook
  (json-mode-hook . lsp)
  (json-mode-hook . tree-sitter-mode)
  (json-mode-hook . prettier-js-mode))

;; YAML

(leaf yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :hook
  (yaml-mode-hook . lsp)
  (yaml-mode-hook . prettier-js-mode))

;; Java

(leaf lsp-java
  ;; https://emacs-lsp.github.io/lsp-java/
  :ensure t
  :hook
  (java-mode-hook . lsp)
  (java-mode-hook . tree-sitter-mode))

(leaf dap-java
  :ensure nil)

;; Kotlin

(leaf kotlin-mode
  ;; https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
  :ensure t
  :hook
  (kotlin-mode-hook . lsp))

;; GraphQL

(leaf graphql-mode
  ;; https://github.com/davazp/graphql-mode
  :ensure t
  :hook
  (graphql-mode-hook . lsp))

;;; init.el ends here
