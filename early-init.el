;;; early-init.el --- -*- lexical-binding:t -*-

;;; Code:
;; Path settings
(add-to-list 'load-path "~/.config/emacs/elisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Set a theme
;;(load-theme 'timu-macos t)
(load-theme 'tron-legacy t)
(setq tron-legacy-theme-vivid-cursor t)

;; Put custom configurations added via M-x customize
;; into its own file
(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Set frame transparency
(set-frame-parameter nil 'alpha-background 85)
(add-to-list 'default-frame-alist '(alpha-background . 85))

;; Start emacs frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setopt inhibit-startup-screen t)                 ; Don't show the startup screen
(setopt visible-bell t)                           ; Enable visible bell
(tool-bar-mode -1)                              ; Don't show the tool bar
(menu-bar-mode -1)                              ; Don't show the menu bar

;; Don't enable package.el
(setopt package-enable-at-startup nil)

;; Don't ask if emacs should follow symlinks
(setopt vc-follow-symlinks t)

(provide 'early-init)
;;; early-init ends here
