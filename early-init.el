
;;; early-init.el --- -*- lexical-binding:t -*-

;;; Code:
(setq gc-cons-threshold 100000000)
(setq package-enable-at-startup nil)
;; Don't ask if emacs should follow symlinks
(setq vc-follow-symlinks t)

(provide 'early-init)
;;; early-init ends here
