;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

(defun my-emacs-path (relative)
  "Return RELATIVE expanded under `user-emacs-directory'."
  (expand-file-name relative user-emacs-directory))

;; Work around libgccjit passing an invalid deployment target on macOS 27.
(when (and (eq system-type 'darwin)
           (not (getenv "MACOSX_DEPLOYMENT_TARGET")))
  (with-temp-buffer
    (when (and (eq 0 (call-process "sw_vers" nil t nil "-productVersion"))
               (progn (goto-char (point-min)) (looking-at "27\\.")))
      (setenv "MACOSX_DEPLOYMENT_TARGET" "27.0"))))

;; The initial graphical frame is created before `init.el' is loaded, so the
;; size saved by `my-save-frame-size' is applied here to avoid a resize jump.
(defvar my-saved-frame-size
  (let ((file (my-emacs-path "frame-size.el")))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (ignore-errors (read (current-buffer))))))
  "Frame size alist read from `frame-size.el', or nil.")

(when (and initial-window-system
           (not (daemonp))
           (consp my-saved-frame-size))
  (setq default-frame-alist
        (append my-saved-frame-size default-frame-alist)))

;;; early-init.el ends here
