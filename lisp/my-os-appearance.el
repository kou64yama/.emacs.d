;;; my-os-appearance.el --- Call functions on OS appearance changes -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Detect OS appearance changes and call registered functions.  On macOS,
;; prefer `ns-system-appearance-change-functions' when available and fall
;; back to polling `defaults read -g' for terminal sessions or older Emacs
;; builds.  Other systems can provide `my-os-appearance-detect-function'.

;;; Code:

(require 'seq)

(defgroup my-os-appearance nil
  "React to OS appearance changes."
  :group 'faces)

(defcustom my-os-appearance-fallback 'dark
  "Appearance to use when detection is unavailable.
Valid values are `dark' and `light'."
  :type '(choice (const :tag "Dark" dark)
                 (const :tag "Light" light))
  :group 'my-os-appearance)

(defcustom my-os-appearance-sync-interval 30
  "Idle interval in seconds to recheck appearance, or nil to disable.
Used as a fallback when native appearance change notifications are
unavailable, and as a safety net on macOS terminal frames."
  :type '(choice (const :tag "Disable timer" nil)
                 (number :tag "Seconds"))
  :group 'my-os-appearance)

(defcustom my-os-appearance-detect-function #'my-os-appearance-detect
  "Function that returns the current OS appearance.
It should return `dark', `light', or nil when unknown."
  :type 'function
  :group 'my-os-appearance)

(defvar my-os-appearance-change-functions nil
  "Abnormal hook run when the OS appearance changes.
Each function receives one argument, APPEARANCE, which is `dark' or
`light'.")

(defvar my-os-appearance--last nil
  "Last appearance passed to `my-os-appearance-change-functions'.")

(defvar my-os-appearance--timer nil
  "Idle timer used to poll OS appearance.")

(defun my-os-appearance-detect ()
  "Return `dark' or `light' for the OS appearance, or nil if unknown."
  (cond
   ((and (boundp 'ns-system-appearance) ns-system-appearance)
    ns-system-appearance)
   ((eq system-type 'darwin)
    (condition-case nil
        (with-temp-buffer
          (when (eq 0 (call-process "defaults" nil t nil "read" "-g"))
            (goto-char (point-min))
            (if (re-search-forward "^[[:space:]]*AppleInterfaceStyle = Dark;" nil t)
                'dark
              'light)))
      (file-error nil)))))

(defun my-os-appearance-current ()
  "Return the current appearance, falling back to `my-os-appearance-fallback'."
  (or (and (functionp my-os-appearance-detect-function)
           (funcall my-os-appearance-detect-function))
      my-os-appearance-fallback))

(defun my-os-appearance--notify (appearance)
  "Run appearance change functions for APPEARANCE when it changed."
  (unless (eq appearance my-os-appearance--last)
    (setq my-os-appearance--last appearance)
    (run-hook-with-args 'my-os-appearance-change-functions appearance)))

;;;###autoload
(defun my-os-appearance-sync (&optional force)
  "Check OS appearance and call change functions if it changed.
With FORCE non-nil, call change functions even if unchanged."
  (interactive "P")
  (let ((appearance (my-os-appearance-current)))
    (when force
      (setq my-os-appearance--last nil))
    (my-os-appearance--notify appearance)))

(defun my-os-appearance--ns-change (appearance)
  "Handle native NS APPEARANCE change notifications."
  (my-os-appearance--notify appearance))

(defun my-os-appearance--native-available-p ()
  "Return non-nil when native NS appearance notifications are available."
  (boundp 'ns-system-appearance-change-functions))

(defun my-os-appearance--setup ()
  "Install appearance change hooks and timers."
  (my-os-appearance--teardown)
  (if (my-os-appearance--native-available-p)
      (add-hook 'ns-system-appearance-change-functions
                #'my-os-appearance--ns-change)
    (when (eq system-type 'darwin)
      (add-function :after after-focus-change-function
                    #'my-os-appearance-sync)))
  (when (and (numberp my-os-appearance-sync-interval)
             (> my-os-appearance-sync-interval 0)
             (or (not (my-os-appearance--native-available-p))
                 (not (display-graphic-p))))
    (setq my-os-appearance--timer
          (run-with-idle-timer my-os-appearance-sync-interval t
                               #'my-os-appearance-sync))))

(defun my-os-appearance--teardown ()
  "Remove appearance change hooks and timers."
  (when my-os-appearance--timer
    (cancel-timer my-os-appearance--timer)
    (setq my-os-appearance--timer nil))
  (remove-function after-focus-change-function #'my-os-appearance-sync)
  (when (boundp 'ns-system-appearance-change-functions)
    (remove-hook 'ns-system-appearance-change-functions
                 #'my-os-appearance--ns-change)))

;;;###autoload
(define-minor-mode my-os-appearance-mode
  "Toggle OS appearance change notifications."
  :global t
  :group 'my-os-appearance
  (if my-os-appearance-mode
      (progn
        (my-os-appearance--setup)
        (my-os-appearance-sync t))
    (my-os-appearance--teardown)))

(provide 'my-os-appearance)

;;; my-os-appearance.el ends here
