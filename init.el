;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Conventions for editing this file:
;; - Keep hand-written configuration in `init.el` organized by `;;;` sections;
;;   `custom.el` is managed by Emacs Customize.
;; - Group package settings in `use-package` declarations (with `:ensure nil`
;;   for built-in packages), keeping each package's options with its package.
;; - `use-package` declaration order: conditions and loading, :preface,
;;   :commands, :mode, :bind, :hook, :custom, :init, then :config.
;; - Prefer `:custom` for customizable variables; reserve `:init`/`:config`
;;   for imperative setup, hooks, and advice.
;; - Resolve paths under `user-emacs-directory` via `my-emacs-path`.
;; - Defer loading where practical and avoid unnecessary eager `require` calls.

;;; Startup and package management

(setq custom-file (my-emacs-path "custom.el"))
(load custom-file :noerror)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; Encrypted authentication

(use-package age
  :demand t
  :custom
  (age-default-identity (expand-file-name "~/.config/age/identity.txt"))
  (age-default-recipient (expand-file-name "~/.config/age/recipients.txt"))
  :config
  (age-file-enable))

(use-package auth-source
  :ensure nil
  :after age
  :custom
  (auth-sources (list (expand-file-name "~/.authinfo.age"))))

;;; System and environment

(use-package alert
  :commands (alert)
  :custom
  (alert-default-style (cond
                        ((eq system-type 'darwin) 'osx-notifier)
                        (t 'message))))

;; Inherit environment variables from shell in graphical frames and daemon.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

(use-package select
  :ensure nil
  :preface
  (defvar my-terminal-last-clipboard nil
    "Last clipboard text exchanged with the macOS terminal clipboard.")
  (defun my-terminal-cut-to-clipboard (text &optional _push)
    "Send TEXT to the macOS clipboard in a terminal frame.
_PUSH is ignored."
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
  :custom
  ;; Share kills and yanks with the graphical desktop clipboard.
  (select-enable-clipboard t)
  :init
  ;; macOS terminal frames do not have the GUI clipboard integration.
  (when (and (eq system-type 'darwin)
             (executable-find "pbcopy")
             (executable-find "pbpaste")
             (not (display-graphic-p)))
    (setq interprogram-cut-function #'my-terminal-cut-to-clipboard
          interprogram-paste-function #'my-terminal-paste-from-clipboard)))

(use-package which-key
  :ensure nil
  :init (which-key-mode 1))

(use-package xt-mouse
  :ensure nil
  :init (xterm-mouse-mode 1))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package autorevert
  :ensure nil
  :init (global-auto-revert-mode 1))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 100)
  (recentf-exclude '("/elpa/" "/\\.git/"))
  :init
  (recentf-mode 1))

(use-package repeat
  :ensure nil
  :init (repeat-mode 1))

;;; Frame size

(use-package my-frame-size
  :ensure nil
  :load-path "lisp"
  :custom
  (my-frame-size-file (my-emacs-path "frame-size.el"))
  :init
  (setq frame-resize-pixelwise t)
  :config
  (my-frame-size-mode 1))

;;; Basic editing and display

(use-package emacs
  :ensure nil
  :preface
  (defun my-enable-line-numbers ()
    "Enable line numbers in the current buffer."
    (display-line-numbers-mode 1))
  :hook ((prog-mode text-mode) . my-enable-line-numbers)
  :custom
  ;; Keep backup files together under the configuration directory.
  (backup-directory-alist `(("." . ,(my-emacs-path "backups/"))))
  (version-control t)
  (delete-old-versions t)
  (kept-old-versions 2)
  (kept-new-versions 6)
  (enable-recursive-minibuffers t)
  (auto-revert-verbose nil)
  :init
  ;; Keep lock files outside the directories being edited.
  (let ((lock-directory (my-emacs-path "locks/")))
    (make-directory lock-directory t)
    (setq lock-file-name-transforms `((".*" ,lock-directory t))))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

(use-package paren
  :ensure nil
  :init (show-paren-mode 1))

(use-package hl-line
  :ensure nil
  :init (global-hl-line-mode 1))

(use-package editorconfig
  :ensure nil
  :init (editorconfig-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package drag-stuff
  :config
  ;; Avoid overriding Org's M-<up>/<down>/<left>/<right> structure commands.
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Appearance

(use-package dashboard
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5) (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package faces
  :ensure nil
  :preface
  (defun my-setup-fonts (&optional frame)
    "Configure fonts for graphical FRAME (or the current frame when nil).
Terminal frames keep the font configured in the terminal emulator."
    (when (display-graphic-p frame)
      (when (find-font (font-spec :family "Andale Mono") frame)
        (set-face-attribute 'default frame :family "Andale Mono" :height 140)
        (set-face-attribute 'fixed-pitch frame :family "Andale Mono"))
      (when (find-font (font-spec :family "Avenir Next") frame)
        (set-face-attribute 'variable-pitch frame :family "Avenir Next"))
      (when (find-font (font-spec :family "Hiragino Sans") frame)
        (dolist (script '(kana han cjk-misc))
          (set-fontset-font t script "Hiragino Sans" frame)))))
  :init
  (when (display-graphic-p)
    (my-setup-fonts))
  (when (daemonp)
    (add-hook 'after-make-frame-functions #'my-setup-fonts)))

(use-package my-auto-theme
  :ensure nil
  :load-path "lisp"
  :demand t
  :custom
  (my-auto-theme-dark-theme 'modus-vivendi)
  (my-auto-theme-light-theme 'modus-operandi)
  (my-auto-theme-fallback 'dark)
  :config
  (my-auto-theme-mode 1))

(use-package all-the-icons)

(use-package all-the-icons-nerd-fonts
  :after all-the-icons
  :config
  (all-the-icons-nerd-fonts-prefer))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  :config
  (doom-modeline-mode 1))

(use-package olivetti
  :preface
  (defvar-local my-olivetti-previous-line-numbers nil
    "Line-number mode state saved before entering Olivetti.")
  (defun my-olivetti-line-numbers ()
    "Hide line numbers in Olivetti and restore their previous state."
    (if olivetti-mode
        (progn
          (setq my-olivetti-previous-line-numbers display-line-numbers-mode)
          (display-line-numbers-mode -1))
      (when my-olivetti-previous-line-numbers
        (display-line-numbers-mode 1))))
  :commands (olivetti-mode)
  :hook (olivetti-mode . my-olivetti-line-numbers))

(use-package mixed-pitch
  :commands (mixed-pitch-mode))

;;; Completion and navigation

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

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-s d" . consult-find)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-y" . consult-yank-pop))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  ;; Terminal child frames are available starting with Emacs 31.
  (when (or (display-graphic-p) (>= emacs-major-version 31))
    (global-corfu-mode 1))
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; Version control

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

(use-package diff-hl
  :demand t
  :hook ((diff-hl-mode . diff-hl-flydiff-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;;; Development and project tools

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  :init
  ;; Larger reads keep LSP traffic from being split into tiny chunks.
  (setq read-process-output-max (* 1024 1024)))

(use-package treemacs
  :defer t
  :bind (("C-x t t" . treemacs)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t d" . treemacs-select-directory)
         ("C-x t B" . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-project-follow-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :after treemacs
  :demand t
  :config
  (treemacs-nerd-icons-config))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

;; Keep agent-shell data (transcripts, screenshots, worktrees) out of project
;; directories and together under `user-emacs-directory'.
(use-package agent-shell
  :preface
  (defun my-agent-shell-dot-subdir (subdir)
    "Return path to agent-shell SUBDIR under `user-emacs-directory'."
    (my-emacs-path (file-name-concat ".agent-shell" subdir)))
  (defun my-agent-shell-subscribe-notifications ()
    "Notify about completed turns and permission requests in inactive shells."
    (let ((shell-buffer (current-buffer)))
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :on-event (lambda (event)
                   (when-let* ((description (pcase (map-elt event :event)
                                              ('turn-complete "Response finished")
                                              ('permission-request "Permission requested")))
                               ((buffer-live-p shell-buffer))
                               ((not (eq shell-buffer
                                         (window-buffer (selected-window))))))
                     (alert (format "%s in %s" description (buffer-name shell-buffer))
                            :title "Agent Shell"))))))
  :commands (agent-shell)
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . agent-shell-submit)
              ("C-c C-k" . agent-shell-interrupt))
  :hook ((agent-shell-mode . mixed-pitch-mode)
         (agent-shell-mode . my-agent-shell-subscribe-notifications))
  :custom
  (agent-shell-dot-subdir-function #'my-agent-shell-dot-subdir))

;; Keep generated HTML previews out of the directories holding Markdown files.
;; Mirror the source path beneath this directory so same-named files in
;; different projects do not overwrite each other.
(use-package markdown-mode
  :preface
  (defvar my-markdown-preview-directory
    (my-emacs-path "markdown-preview")
    "Directory where Markdown HTML previews are written.")

  (defun my-markdown-export-file-name (original &optional extension)
    "Relocate the export path returned by ORIGINAL to `my-markdown-preview-directory'.
Keep ORIGINAL's extension handling and its nil result for non-file buffers.
Mirror the absolute path so same-named files in different projects do not
overwrite each other.  EXTENSION is passed through to ORIGINAL."
    (when-let* ((name (funcall original extension)))
      (let* ((absolute (expand-file-name name))
             (target (expand-file-name
                      (string-trim-left absolute "/")
                      my-markdown-preview-directory)))
        (make-directory (file-name-directory target) t)
        target)))
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . olivetti-mode)
         (markdown-mode . mixed-pitch-mode))
  :custom
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; Export, export-and-preview, and live preview all use this function.
  (advice-add 'markdown-export-file-name :around #'my-markdown-export-file-name))

;;; Org

(use-package org
  :ensure nil
  :preface
  (defvar my-org-directory (expand-file-name "~/org/")
    "Directory for Org files.")
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook ((org-mode . olivetti-mode)
         (org-mode . mixed-pitch-mode))
  :custom
  (org-directory my-org-directory)
  (org-default-notes-file (expand-file-name "inbox.org" my-org-directory))
  (org-agenda-files (list my-org-directory))
  (org-capture-templates
   '(("t" "Task" entry (file+headline "inbox.org" "Tasks")
      "* TODO %?\n%a")
     ("n" "Note" entry (file+headline "inbox.org" "Notes")
      "* %?\n%U\n%a")
     ("a" "Appointment" entry (file "gcal.org")
      "* %?\n:PROPERTIES:\n:calendar-id: primary\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n"
      :jump-to-captured t)))
  ;; Display
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  ;; Editing
  (org-return-follows-link t)
  (org-M-RET-may-split-line nil)
  (org-insert-heading-respect-content t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-use-fast-todo-selection t)
  (org-log-done 'time)
  (org-log-into-drawer t))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autolinks nil)
  (org-appear-inside-latex t))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-list '((43 . "◦") (45 . "•") (42 . "⁃"))))

;;;; Japanese holidays (calendar and Org agenda)

(use-package japanese-holidays
  :after calendar
  :init
  (setq org-agenda-include-diary t)
  :config
  ;; Do not append to `calendar-holidays': its default list contains US
  ;; holidays such as Thanksgiving, and this config may run twice.
  (setq calendar-holidays (append japanese-holidays
                                  holiday-local-holidays
                                  holiday-other-holidays)
        calendar-mark-holidays-flag t))

;;;; Google Calendar (org-gcal)

(use-package org-gcal
  :defer t
  :preface
  (defvar my-oauth-age-file (my-emacs-path "oauth2-auto.age")
    "Age-encrypted OAuth token store.")

  (defun my-oauth-age-read-all ()
    "Read the age-encrypted OAuth token alist."
    (if (not (file-exists-p my-oauth-age-file))
        (if (file-exists-p (my-emacs-path "oauth2-auto.plist"))
            (user-error "Legacy OAuth store exists; move it aside before reauthorizing")
          nil)
      (with-temp-buffer
        (insert-file-contents my-oauth-age-file)
        (let ((read-eval nil))
          (read (current-buffer))))))

  (defun my-oauth-age-read (username provider)
    "Read the OAuth token for USERNAME and PROVIDER."
    (alist-get (list username provider) (my-oauth-age-read-all) nil nil #'equal))

  (defun my-oauth-age-write (username provider plist)
    "Store PLIST for USERNAME and PROVIDER in the age-encrypted store."
    (let* ((entries (my-oauth-age-read-all))
           (temporary (make-temp-file (my-emacs-path ".oauth2-auto-") nil ".age")))
      (unwind-protect
          (progn
            (setf (alist-get (list username provider) entries nil nil #'equal) plist)
            (with-temp-buffer
              (let ((print-length nil) (print-level nil))
                (prin1 entries (current-buffer)))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (point-min) (point-max) temporary nil 'silent)))
            (set-file-modes temporary #o600)
            (rename-file temporary my-oauth-age-file t)
            plist)
        (when (file-exists-p temporary) (delete-file temporary)))))

  (defvar my-org-gcal--credentials-loaded nil
    "Non-nil after auth-source credentials have been read.")

  (defun my-org-gcal-ensure-auth (&rest _)
    "Load Google OAuth credentials from auth-source and register them.
Arguments are ignored so this can be used as :before advice."
    (require 'auth-source)
    ;; Read the encrypted auth file only once per session.
    (unless my-org-gcal--credentials-loaded
      (setq org-gcal-client-id
            (auth-source-pick-first-password :host "gcal-client-id")
            org-gcal-client-secret
            (auth-source-pick-first-password :host "gcal-client-secret"))
      (when (and org-gcal-client-id org-gcal-client-secret)
        (setq my-org-gcal--credentials-loaded t)))
    (unless (and org-gcal-client-id org-gcal-client-secret)
      (user-error "Add gcal-client-id and gcal-client-secret to authinfo.age"))
    (org-gcal-reload-client-id-secret))

  (defun my-org-gcal-save (&rest _)
    "Persist org-gcal fetch files after the current update."
    (run-at-time
     0 nil
     (lambda ()
       (dolist (file (mapcar #'cdr org-gcal-fetch-file-alist))
         (let ((buffer (get-file-buffer file)))
           (when (and buffer (buffer-modified-p buffer))
             (with-current-buffer buffer (save-buffer))))))))
  :bind (("C-c g f" . org-gcal-fetch)
         ("C-c g s" . org-gcal-sync)
         ("C-c g p" . org-gcal-post-at-point))
  :custom
  (org-gcal-fetch-file-alist
   (list (cons "primary" (expand-file-name "gcal.org" my-org-directory))))
  (org-gcal-local-timezone "Asia/Tokyo")
  (org-gcal-notify-p t)
  (org-gcal-up-days 30)
  (org-gcal-down-days 90)
  (org-gcal-remove-api-cancelled-events nil)
  (org-gcal-strip-html-descriptions t)
  :init
  ;; Non-nil placeholders avoid org-gcal's load-time warning. The real values
  ;; are substituted by `my-org-gcal-ensure-auth' before any request.
  (setq org-gcal-client-id "not-set"
        org-gcal-client-secret "not-set")
  :config
  ;; oauth2-auto otherwise encrypts its token store using GPG/plstore.
  (advice-add 'oauth2-auto--plstore-read :override #'my-oauth-age-read)
  (advice-add 'oauth2-auto--plstore-write :override #'my-oauth-age-write)
  ;; `org-gcal-fetch' delegates to `org-gcal-sync', so advising sync covers
  ;; both. Also cover the hooks that post/refile/delete entries automatically.
  (dolist (command '(org-gcal-sync
                     org-gcal-sync-buffer
                     org-gcal-post-at-point
                     org-gcal-delete-at-point))
    (advice-add command :before #'my-org-gcal-ensure-auth))

  ;; org-gcal inserts fetched events into the buffer but never saves it.
  ;; Defer the save so it runs outside `atomic-change-group'.
  (add-to-list 'org-gcal-after-update-entry-functions #'my-org-gcal-save))

;;; init.el ends here
