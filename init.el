;;; init.el --- -*- lexical-binding: t -*-

;; Some path settings
;;(add-to-list 'load-path "/opt/share/emacs/site-lisp/org/")
(add-to-list 'load-path "~/.config/emacs/elisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-file "~/.config/emacs/custom-settings.el")
(load custom-file t)

;; Set up user info
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com")
(setenv "SHELL" "/opt/bin/fish")

;; Start up maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Some general settings
;; Don't show the startup screen.
(setq inhibit-startup-screen t)
;; Don't show the tool bar or menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Show the time in the modeline
(display-time-mode 1)
(column-number-mode 1)
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq-default tab-always-indent t
              indent-tabs-mode nil
              tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Backup settings
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; History settings
(setq savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)
(setq history-delete-duplicates t)
(setq history-length 50
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
(put 'ido-buffer-history                    'history-length 50)
(put 'ido-file-history                      'history-length 50)
(put 'minibuffer-history                    'history-length 50)
(put 'kill-ring                             'history-length 50)

;; Keybindings
(global-set-key [mouse-6] 'backward-word)
(global-set-key [mouse-7] 'forward-word)
(global-set-key [mouse-8] 'scroll-up-command)
(global-set-key [mouse-9] 'scroll-down-command)
(global-set-key [mouse-10] 'list-buffers)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 115 :width normal)))))

;; Set a theme
(load-theme 'tron-legacy t)

;; Package management setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; Install use-package if it's not installed already
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
(use-package diminish :ensure t)

;; Company mode
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3)
  :config
  (global-company-mode 1))
(add-hook 'after-init-hook 'global-company-mode)

;; Org mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'turn-on-font-lock)
(setq org-agenda-files (list "~/org/todo.org")
      org-directory "~/org/"
      org-enable-github-support t
      org-enable-journal-support t
      org-use-speed-commands t
      org-projectile-file "~/org/todo.org"
      org-default-notes-file "~/org/notes.org"
      org-journal-dir "~/org/journal/"
      org-log-done 'time-date
      org-startup-truncated nil
      calendar-latitude 33.0752523
      calendar-longitude -80.0220569)
(setq org-ref-bibliography-notes "~/org/bibtex/notes.org"
      org-ref-default-bibliography '("~/org/bibtex/library.bib")
      org-ref-pdf-directory "~/org/bibtex/bibtex-pdfs")

;; Org-mode key bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(require 'org-tempo)

;; Org tags
(setq org-tag-alist '(("WORK" . ?W)
                      ("home" . ?h)
                      ("lab" . ?l)
                      ("research" . ?r)
                      ("dogs" . ?d)
                      ("radioclub" . ?C)))

;; Org capture templates
(setq org-capture-templates '(("t" "Todo"
                               entry (file+headline "~/org/todo.org" "Tasks")
                               "** TODO %?\n %i\n %a")
                              ("a" "Appointment"
                               entry (file+headline "~/org/todo.org" "Calendar")
                               "** APPT %^{Description} %^g\n %?\n Added: %U")
                              ("j" "Journal"
                               entry (file+olp+datetree "~/org/journal/journal.org")
                               "* %?\nEntered on %U\n %i\n %a")
                              ("n" "Notes"
                               entry (file+olp+datetree "~/org/notes.org")
                               "* %^{Description} %^g %?\n Added: %U")
                              ("l" "Lab book"
                               entry (file+olp+datetree "~/org/PhD/notes.org")
                               "* %U\n %?\n %i\n %a")))

;; Magit
(use-package magit)

;; Projectile
(use-package projectile
  :config
  (projectile-mode t))

(provide 'init)
;;; init.el ends here
