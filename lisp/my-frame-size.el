;;; my-frame-size.el --- Save and restore graphical frame size -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Persist the last normal graphical frame size to `my-frame-size-file' on
;; frame deletion and Emacs exit, and restore it on startup and for new
;; graphical frames in daemon sessions.

;;; Code:

(defgroup my-frame-size nil
  "Save and restore graphical frame size."
  :group 'frames)

(defcustom my-frame-size-file (locate-user-emacs-file "frame-size.el")
  "File where the last graphical frame size is saved."
  :type 'file
  :group 'my-frame-size)

(defvar my-saved-frame-size nil
  "Frame size alist read from `my-frame-size-file', or nil.")

(defun my-frame-size--load ()
  "Load saved frame size from `my-frame-size-file' into `my-saved-frame-size'."
  (when (file-exists-p my-frame-size-file)
    (with-temp-buffer
      (insert-file-contents my-frame-size-file)
      (goto-char (point-min))
      (setq my-saved-frame-size (ignore-errors (read (current-buffer)))))))

;;;###autoload
(defun my-frame-size-save (&optional frame)
  "Save FRAME's size for the next session.
FRAME defaults to the selected frame.  Ignore non-graphical frames, child
frames, and frames in fullscreen or maximized state, so the last normal
size is kept when Emacs exits fullscreen."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame)
               (display-graphic-p frame)
               (null (frame-parameter frame 'parent-frame))
               (null (frame-parameter frame 'fullscreen)))
      (let ((size (list (cons 'width (frame-width frame))
                        (cons 'height (frame-height frame)))))
        (setq my-saved-frame-size size)
        (with-temp-file my-frame-size-file
          (insert ";;; -*- lexical-binding: t; -*-\n")
          (prin1 size (current-buffer))
          (insert "\n"))))))

;;;###autoload
(defun my-frame-size-apply (&optional frame)
  "Apply the saved size to graphical FRAME (defaulting to the selected frame)."
  (let ((frame (or frame (selected-frame))))
    (when (and (consp my-saved-frame-size)
               (display-graphic-p frame)
               (null (frame-parameter frame 'parent-frame)))
      (set-frame-size frame
                      (alist-get 'width my-saved-frame-size)
                      (alist-get 'height my-saved-frame-size)))))

;;;###autoload
(define-minor-mode my-frame-size-mode
  "Toggle saving and restoring graphical frame size."
  :global t
  :group 'my-frame-size
  (remove-hook 'kill-emacs-hook #'my-frame-size-save)
  (remove-hook 'delete-frame-functions #'my-frame-size-save)
  (remove-hook 'window-setup-hook #'my-frame-size-apply)
  (remove-hook 'after-make-frame-functions #'my-frame-size-apply)
  (when my-frame-size-mode
    (my-frame-size--load)
    (add-hook 'kill-emacs-hook #'my-frame-size-save)
    (add-hook 'delete-frame-functions #'my-frame-size-save)
    ;; Re-apply after startup finishes so font and tab-bar changes do not
    ;; shrink the initial frame's row/column count.
    (add-hook 'window-setup-hook #'my-frame-size-apply)
    ;; Leave terminal frames alone: apply the saved size only to new GUI frames.
    (when (daemonp)
      (add-hook 'after-make-frame-functions #'my-frame-size-apply))))

(provide 'my-frame-size)

;;; my-frame-size.el ends here
