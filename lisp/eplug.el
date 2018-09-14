;;; eplug.el -- Package manager
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(eval-when-compile (require 'cl))
(require 'package)

(defvar eplug-initialized nil)
(defvar eplug-bundled-list nil)

(defcustom eplug-last-refresh nil
  "Last refreshed time."
  :group 'eplug)

(defun eplug-init ()
  "Initialize."
  (unless eplug-initialized
    (setq eplug-initialized t)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)))

(defun eplug (pkg)
  "Bundle PKG."
  (add-to-list 'eplug-bundled-list pkg))

(defun eplug-check ()
  "Check uninstalled packages."
  (mapcan (lambda (pkg)
	    (unless (package-installed-p pkg)
	      (list pkg)))
	  eplug-bundled-list))

(defun eplug-install ()
  "Install bundled packages."
  (dolist (pkg (eplug-check))
    (eplug-refresh)
    (package-install pkg)))

(defun eplug-refresh ()
  "Refresh package contents."
  (let ((now (float-time)))
    (when (or (not eplug-last-refresh)
              (> (- now eplug-last-refresh) 86400))
      (package-refresh-contents)
      (customize-save-variable 'eplug-last-refresh (float-time)))))

(provide 'eplug)
;;; eplug.el ends here
