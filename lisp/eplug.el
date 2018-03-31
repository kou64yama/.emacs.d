;;; eplug.el -- Package manager
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(eval-when-compile (require 'cl))
(require 'package)

(defvar eplug-bundled nil)

(defcustom eplug-last-refresh nil
  ""
  :group 'eplug)

(defun eplug (pkg)
  ""
  (add-to-list 'eplug-bundled pkg))

(defun eplug-check ()
  ""
  (mapcan (lambda (pkg)
	    (unless (package-installed-p pkg)
	      (list pkg)))
	  eplug-bundled))

(defun eplug-install ()
  ""
  (dolist (pkg (eplug-check))
    (eplug-refresh)
    (package-install pkg)))

(defun eplug-refresh ()
  ""
  (let ((now (float-time)))
    (when (or (not eplug-last-refresh)
              (> (- now eplug-last-refresh) 86400))
      (package-refresh-contents)
      (customize-save-variable 'eplug-last-refresh (float-time)))))

(provide 'eplug)
;;; eplug.el ends here
