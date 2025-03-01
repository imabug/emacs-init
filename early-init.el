;;; early-init.el --- -*- lexical-binding:t -*-

;;; Code:
;; Path settings
(add-to-list 'load-path "~/.config/emacs/elisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Set a theme
(load-theme 'timu-macos t)
;; (setq tron-legacy-theme-vivid-cursor t)

;; Don't enable package.el
(setq package-enable-at-startup nil)

;; Don't ask if emacs should follow symlinks
(setq vc-follow-symlinks t)

;; Put custom configurations added via M-x customize
;; into its own file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Start emacs frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)                 ; Don't show the startup screen
(setq visible-bell t)                           ; Enable visible bell
(tool-bar-mode -1)                              ; Don't show the tool bar
(menu-bar-mode -1)                              ; Don't show the menu bar

(provide 'early-init)
;;; early-init ends here
