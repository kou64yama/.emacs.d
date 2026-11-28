;;; my-auto-theme.el --- Switch between dark and light themes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Automatically switch between a configured dark theme and light theme
;; based on the system appearance (currently macOS, including terminal
;; sessions), falling back to a configurable default when detection is
;; unavailable.

;;; Code:

(require 'seq)

(defgroup my-auto-theme nil
  "Automatic Dark/Light theme switching."
  :group 'faces)

(defcustom my-auto-theme-dark-theme 'modus-vivendi
  "Theme symbol to load when using the dark appearance."
  :type 'symbol
  :group 'my-auto-theme)

(defcustom my-auto-theme-light-theme 'modus-operandi
  "Theme symbol to load when using the light appearance."
  :type 'symbol
  :group 'my-auto-theme)

(defcustom my-auto-theme-fallback 'dark
  "Appearance to use when the system appearance cannot be detected.
Valid values are `dark' and `light'."
  :type '(choice (const :tag "Dark" dark)
                 (const :tag "Light" light))
  :group 'my-auto-theme)

(defcustom my-auto-theme-sync-interval 30
  "Idle interval in seconds to check system appearance, or nil to disable."
  :type '(choice (const :tag "Disable timer" nil)
                 (number :tag "Seconds"))
  :group 'my-auto-theme)

(defvar my-auto-theme--timer nil
  "Idle timer used to poll system appearance.")

(defun my-auto-theme--detect-appearance ()
  "Return `dark' or `light' for the system appearance, or nil if unknown."
  (when (eq system-type 'darwin)
    (condition-case nil
        (with-temp-buffer
          (when (eq 0 (call-process "defaults" nil t nil "read" "-g"))
            (goto-char (point-min))
            (if (re-search-forward "^[[:space:]]*AppleInterfaceStyle = Dark;" nil t)
                'dark
              'light)))
      (file-error nil))))

(defun my-auto-theme--resolve-theme ()
  "Return the theme symbol to apply based on appearance or fallback."
  (let ((appearance (or (my-auto-theme--detect-appearance)
                        my-auto-theme-fallback)))
    (pcase appearance
      ('light my-auto-theme-light-theme)
      (_ my-auto-theme-dark-theme))))

(defun my-auto-theme--apply (theme)
  "Enable THEME unless already active, disabling the other managed theme."
  (when (and theme (not (memq theme custom-enabled-themes)))
    (let ((managed (delq nil (list my-auto-theme-dark-theme
                                   my-auto-theme-light-theme))))
      (mapc #'disable-theme
            (seq-filter (lambda (enabled) (memq enabled managed))
                        custom-enabled-themes)))
    (load-theme theme t)))

;;;###autoload
(defun my-auto-theme-sync ()
  "Apply the theme matching the system appearance or fallback."
  (interactive)
  (when-let* ((theme (my-auto-theme--resolve-theme)))
    (my-auto-theme--apply theme)))

;;;###autoload
(define-minor-mode my-auto-theme-mode
  "Toggle automatic Dark/Light theme switching."
  :global t
  :group 'my-auto-theme
  (when my-auto-theme--timer
    (cancel-timer my-auto-theme--timer)
    (setq my-auto-theme--timer nil))
  (remove-function after-focus-change-function #'my-auto-theme-sync)
  (when my-auto-theme-mode
    (my-auto-theme-sync)
    (when (eq system-type 'darwin)
      (add-function :after after-focus-change-function
                    #'my-auto-theme-sync)
      (when (and (numberp my-auto-theme-sync-interval)
                 (> my-auto-theme-sync-interval 0))
        (setq my-auto-theme--timer
              (run-with-idle-timer my-auto-theme-sync-interval t
                                   #'my-auto-theme-sync))))))

(provide 'my-auto-theme)

;;; my-auto-theme.el ends here
