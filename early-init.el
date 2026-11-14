;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Work around libgccjit passing an invalid deployment target on macOS 27.
(when (and (eq system-type 'darwin)
           (not (getenv "MACOSX_DEPLOYMENT_TARGET")))
  (with-temp-buffer
    (when (and (eq 0 (call-process "sw_vers" nil t nil "-productVersion"))
               (progn (goto-char (point-min)) (looking-at "27\\.")))
      (setenv "MACOSX_DEPLOYMENT_TARGET" "27.0"))))

;;; early-init.el ends here
