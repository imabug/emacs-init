;;; init.el -- -*- lexical-binding: t -*-

;;; Code:
;; Some path settings
(add-to-list 'load-path "~/.config/emacs/elisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Set up user info
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com"
      user-login-name "eugenem")
(setenv "SHELL" "/opt/bin/fish")

;; Start emacs frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Some general settings
(setq inhibit-startup-screen t)            ;Don't show the startup screen
(tool-bar-mode -1)                         ;Don't show the tool bar
(menu-bar-mode -1)                         ;Don't show the menu bar
;; Show the time in the modeline
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode 1)                     ; Show column numbers
(show-paren-mode 1)
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; Set tab behaviour
(setq-default tab-always-indent t
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(global-display-line-numbers-mode t)       ;Display line numbers
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; Set up some fonts
(defvar em/default-font-size 110)
(defvar em/default-variable-font-size 110)
(set-face-attribute 'default nil :font "Hack" :height em/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Hack" :height em/default-font-size)
(set-face-attribute 'variable-pitch nil
                    :font "Cantarell"
                    :height em/default-variable-font-size
                    :weight 'regular)
;; Backup settings
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list" t)))
;; History settings
(setq savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)
(setq history-delete-duplicates t
      history-length 50
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))
(put 'savehist-minibuffer-history-variables 'history-length 50)
(put 'extended-command-history              'history-length 50)
(put 'command-history                       'history-length 50)
(put 'query-replace-history                 'history-length 50)
(put 'org-read-date-history                 'history-length 50)
(put 'org-table-formula-history             'history-length 50)
(put 'helm-M-x-input-history                'history-length 50)
(put 'minibuffer-history                    'history-length 50)
(put 'kill-ring                             'history-length 50)
;; Set a theme
(load-theme 'tron-legacy t)
(setq tron-legacy-theme-vivid-cursor t)

;; Package management setup
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
(use-package diminish
  :ensure t)

(provide 'init)
;;; init.el ends here
