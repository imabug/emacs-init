;;; early-init.el --- -*- lexical-binding:t -*-

;;; Code:
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)
;; Don't ask if emacs should follow symlinks
(setq vc-follow-symlinks t)
;; Don't enable package.el
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init ends here
