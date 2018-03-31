;;; sensible.el -- Sensible configurations
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(setq backup-inhibited t
      column-number-mode t
      inhibit-startup-message -1
      visible-bell t)

(defun sensible-after-init-hook ()
  (tool-bar-mode -1)
  (when window-system
    (scroll-bar-mode -1))
  (unless window-system
    (menu-bar-mode -1)))

(add-to-list 'after-init-hook 'sensible-after-init-hook)

(provide 'sensible)
;;; sensible.el ends here
