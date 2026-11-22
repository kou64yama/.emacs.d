;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;;; Startup and package management

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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
  (age-default-identity (expand-file-name "age/identity.txt" user-emacs-directory))
  (age-default-recipient (expand-file-name "age/recipients.txt" user-emacs-directory))
  :config
  (age-file-enable)
  (setq auth-sources (list (expand-file-name "authinfo.age" user-emacs-directory))))

(defvar my-oauth-age-file (expand-file-name "oauth2-auto.age" user-emacs-directory)
  "Age-encrypted OAuth token store.")

(defun my-oauth-age-read-all ()
  "Read the age-encrypted OAuth token alist."
  (if (not (file-exists-p my-oauth-age-file))
      (if (file-exists-p (expand-file-name "oauth2-auto.plist" user-emacs-directory))
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
         (temporary (make-temp-file
                     (expand-file-name ".oauth2-auto-" user-emacs-directory)
                     nil ".age")))
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

;;; Basic editing and display

;; Keep backup files together, without hard-coding a home directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      version-control t
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(xterm-mouse-mode 1)
;; Show line numbers in programming and text buffers.
(defun my-enable-line-numbers ()
  (display-line-numbers-mode 1))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'my-enable-line-numbers))

(show-paren-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)
(global-auto-revert-mode 1)

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 100)
  (recentf-exclude '("/elpa/" "/\\.git/"))
  :init
  (recentf-mode 1))

;; Prefer JetBrains Mono in graphical frames. Terminal frames keep the
;; font configured in the terminal emulator.
(when (display-graphic-p)
  (when (find-font (font-spec :family "JetBrains Mono"))
    (set-face-attribute 'default nil :family "JetBrains Mono" :height 140)
    (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono"))
  (when (find-font (font-spec :family "Avenir Next"))
    (set-face-attribute 'variable-pitch nil :family "Avenir Next"))
  ;; Use Hiragino Sans for Japanese text when available.
  (when (find-font (font-spec :family "Hiragino Sans"))
    (dolist (script '(kana han cjk-misc))
      (set-fontset-font t script "Hiragino Sans"))))

;; These packages are built in starting with Emacs 30.
(if (< emacs-major-version 30)
    (use-package editorconfig
      :ensure t
      :config (editorconfig-mode 1))
  (use-package editorconfig
    :ensure nil
    :config (editorconfig-mode 1)))

(if (< emacs-major-version 30)
    (use-package which-key
      :init (which-key-mode 1))
  (use-package which-key
    :ensure nil
    :init (which-key-mode 1)))

;;; Environment and clipboard

;; Inherit environment variables from shell in graphical frames and daemon.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :config
  (exec-path-from-shell-initialize))

;; Share kills and yanks with the graphical desktop clipboard.
(setq select-enable-clipboard t)

;; macOS terminal frames do not have the GUI clipboard integration.
(when (and (eq system-type 'darwin)
           (executable-find "pbcopy")
           (executable-find "pbpaste"))
  (defvar my-terminal-last-clipboard nil
    "Last clipboard text exchanged with the macOS terminal clipboard.")
  (defun my-terminal-cut-to-clipboard (text &optional push)
    "Send TEXT to the macOS clipboard in a terminal frame.
PUSH is ignored."
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
  (unless (display-graphic-p)
    (setq interprogram-cut-function #'my-terminal-cut-to-clipboard
          interprogram-paste-function #'my-terminal-paste-from-clipboard)))

;;; Appearance

(load-theme 'modus-vivendi t)

(if (eq system-type 'darwin)
    (progn
      (defun my-update-macos-theme ()
        "Match the Modus theme to the macOS global appearance."
        (with-temp-buffer
          (when (zerop (call-process "defaults" nil t nil "read" "-g"))
            (goto-char (point-min))
            (let ((theme (if (re-search-forward
                              "^[[:space:]]*AppleInterfaceStyle = Dark;" nil t)
                             'modus-vivendi
                           'modus-operandi)))
              (unless (memq theme custom-enabled-themes)
                (mapc #'disable-theme
                      (seq-filter (lambda (enabled)
                                    (memq enabled '(modus-vivendi modus-operandi)))
                                  custom-enabled-themes))
                (load-theme theme t))))))
      (run-with-timer 0 5 #'my-update-macos-theme))
  (use-package auto-dark
    :custom
    (auto-dark-themes '((modus-vivendi) (modus-operandi)))
    :config
    (auto-dark-mode 1)))

(use-package doom-modeline
  :custom
  (doom-modeline-icon nil)
  :config
  (doom-modeline-mode 1))

(use-package olivetti
  :hook ((markdown-mode . olivetti-mode)
         (org-mode . olivetti-mode)))

(use-package mixed-pitch
  :hook ((markdown-mode . mixed-pitch-mode)
         (org-mode . mixed-pitch-mode)))

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
    (global-corfu-mode 1)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;; Development and project tools

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit)

;; Keep agent-shell data (transcripts, screenshots, worktrees) out of project
;; directories and together under `user-emacs-directory'.
(use-package agent-shell
  :preface
  (defun my-agent-shell-dot-subdir (subdir)
    "Return path to agent-shell SUBDIR under `user-emacs-directory'."
    (expand-file-name (file-name-concat ".agent-shell" subdir)
                      user-emacs-directory))
  :commands (agent-shell)
  :custom
  (agent-shell-dot-subdir-function #'my-agent-shell-dot-subdir))

;; Keep generated HTML previews out of the directories holding Markdown files.
;; Mirror the source path beneath this directory so same-named files in
;; different projects do not overwrite each other.
(use-package markdown-mode
  :preface
  (defvar my-markdown-preview-directory
    (expand-file-name "markdown-preview" user-emacs-directory)
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
  :custom
  (markdown-hide-markup t)
  (markdown-fontify-code-blocks-natively t)
  :config
  ;; Export, export-and-preview, and live preview all use this function.
  (advice-add 'markdown-export-file-name :around #'my-markdown-export-file-name))

;;; Org
(defvar my-org-directory (expand-file-name "~/org/")
  "Directory for Org files.")

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
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

;;; Japanese holidays (calendar and Org agenda)
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

;;; Google Calendar (org-gcal)
(use-package org-gcal
  :defer t
  :preface
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
  :init
  (setq org-gcal-fetch-file-alist
        (list (cons "primary" (expand-file-name "gcal.org" my-org-directory)))
        org-gcal-local-timezone "Asia/Tokyo"
        ;; Show OAuth/completion prompts in the minibuffer, not Notification Center.
        org-gcal-notify-p t
        alert-default-style 'message
        org-gcal-up-days 30
        org-gcal-down-days 90
        org-gcal-remove-api-cancelled-events nil
        org-gcal-strip-html-descriptions t)
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

;;; Editing and version control

(use-package diff-hl
  :demand t
  :hook ((diff-hl-mode . diff-hl-flydiff-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Web browsing

(use-package eww
  :ensure nil
  :custom
  ;; Use DuckDuckGo HTML version for fast, clean text search.
  (eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  ;; Rename buffer based on page title/URL so multiple pages can be opened.
  (eww-auto-rename-buffer t)
  (eww-bookmarks-directory (expand-file-name "eww-bookmarks" user-emacs-directory))
  :bind (("C-c w w" . eww)
         ("C-c w s" . eww-search-words)
         :map eww-mode-map
         ("r" . eww-reload)
         ("h" . eww-back-url)
         ("l" . eww-forward-url)
         ("o" . eww)))

(use-package shr
  :ensure nil
  :custom
  (shr-use-fonts t)
  (shr-use-colors t)
  ;; Constrain inline image size relative to window width.
  (shr-max-image-proportion 0.6)
  (shr-image-animate t)
  (shr-width nil))

;; Prefer EWW inside Emacs, with external browser fallback.
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function #'eww-browse-url)
  (browse-url-secondary-browser-function #'browse-url-default-browser)
  ;; Direct complex or media-heavy pages directly to system browser.
  (browse-url-handlers
   '(;; OAuth must open in a real browser so the localhost callback can fire.
     ("https?://accounts\\.google\\.com/" . browse-url-default-browser)
     ("https?://\\(www\\.\\)?youtube\\.com" . browse-url-default-browser)
     ("https?://github\\.com/.*/\\(issues\\|pull\\)" . browse-url-default-browser))))

;;; init.el ends here
