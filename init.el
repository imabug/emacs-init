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

;; Set a theme
(load-theme 'tron-legacy t)

;; Package management setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/")))
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
(with-eval-after-load 'org
  (org-defkey org-mode-map [(meta return)] 'org-meta-return))
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar #'(lambda (file)
              (when (file-exists-p file)
                (push file org-agenda-files)))
          (org-projectile-todo-files)))

;; Magit
(use-package magit)

;; Projectile
(use-package projectile
  :config
  (projectile-mode t))

;; Treemacs
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-display-in-side-window t)
  (treemacs-file-event-delay 5000)
  (treemacs-file-follow-delay 0.2)
  (treemacs-follow-after-init t)
  (treemacs-follow-recenter-distance 0.1)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-desc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 35)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map ("C-p" . treemacs-previous-line)))
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))
(use-package treemacs-projectile
  :defer t
  :after (treemacs projectile))

;; Smartparens
(use-package smartparens
  :hook (prog-mode .smartparens-mode)
  :diminish smartparens-mode)
(show-paren-mode t)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; LSP Language Server Protocol
(use-package lsp-mode
  :bind
  (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-prefer-flymake nil
        lsp-keymap-prefix "C-x l"
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        ;; lock files will kill `npm start'
        create-lockfiles nil)
  :hook
  ((java-mode python-mode go-mode rust-mode
              js-mode js2-mode typescript-mode web-mode
              c-mode c++-mode objc-mode) . lsp-deferred))
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
    (lsp-ui-doc-background ((t (:background nil))))
    (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
    :bind
    (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references)
          ("C-c u" . lsp-ui-imenu)
          ("M-i" . lsp-ui-doc-focus-frame))
    (:map lsp-mode-map
          ("M-n" . forward-paragraph)
          ("M-p" . backward-paragraph))
    :custom
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-border (face-foreground 'default))
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-code-actions nil)
    :config
    ;; Use lsp-ui-doc-webkit only in GUI
    (when (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
    ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
    ;; https://github.com/emacs-lsp/lsp-ui/issues/243
    (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
      (setq mode-line-format nil))
    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))
;; (use-package dap-mode
;;   :diminish
;;   :bind
;;   (:map dap-mode-map
;;         (("<f12>" . dap-debug)
;;          ("<f8>" . dap-continue)
;;          ("<f9>" . dap-next)
;;          ("<M-f11>" . dap-step-in)
;;          ("C-M-<f11>" . dap-step-out)
;;          ("<f7>" . dap-breakpoint-toggle))))

;; Languages
;; PHP
(use-package php-mode :ensure t)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))

;; Web-mode
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\'" ".php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


(provide 'init)
;;; init.el ends here
