;;; font.el -- Font Utilities
;;; Commentary:
;;; Code:

(face-attribute 'default :family)

(defcustom font/default-height (face-attribute 'default :height)
  ""
  :group 'font)

(defun font/set-height (height)
  ""
  (interactive)
  (when (< 0 height)
    (set-face-attribute 'default nil :height height)))

(defun font/reset-height ()
  ""
  (interactive)
  (font/set-height font/default-height))

(defun font/get-height ()
  ""
  (interactive)
  (face-attribute 'default :height))

(defun font/increase-height ()
  ""
  (interactive)
  (font/set-height (+ (font/get-height) 10)))

(defun font/decrease-height ()
  ""
  (interactive)
  (font/set-height (- (font/get-height) 10)))

(provide 'font)
;;; font.el ends here
