;;; sensible.el --- Sensible configurations.
;;; Commentary:

;; Copyright (c) 2016-present YAMADA Koji.  All rights reserved.

;; This source code is licensed under the MIT license found in the
;; LICENSE file in the root directory of this source tree.

;;; Code:

(setq backup-inhibited t)
(setq visible-bell t)
(setq inhibit-startup-message -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)

(provide 'sensible)
;;; sensible.el ends here
