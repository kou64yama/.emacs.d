;;; package-bundle.el --- Package manager
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.  All rights reserved.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

;; Define `package-bundled-packages`.
(defvar package-bundled-packages nil)

;; Define `package-bundle`.
(defun package-bundle (pkg)
  (add-to-list 'package-bundled-packages pkg t))

;; Define `package-bundle-sync`.
(defun package-bundle-sync ()
  (setq package-selected-packages package-bundled-packages)
  (package-install-selected-packages))

(provide 'package-bundle)
;;; packabe-bundle.el ends here
