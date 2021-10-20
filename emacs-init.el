;;; init.el --- -*- lexical-binding: t -*-

;; Some path settings
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "~/.config/emacs/pkg/")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(setq custom-file "~/.config/emacs/custom-settings.el")
(load custom-file t)

;; Set some personal information
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com")
(setenv "SHELL" "/opt/bin/fish")

;; Package setup
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t
      load-prefer-newer t)
(require 'use-package)
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

;; General settings
;; Set tab behaviour
(setq-default tab-always-indent t
              indent-tabs-mode nil
              tab-width 4)
;; Display time in the mode line
(display-time-mode 1)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(column-number-mode 1)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; Establish some keybindings
(global-set-key [mouse-6] 'backward-word)
(global-set-key [mouse-7] 'forward-word)
(global-set-key [mouse-8] 'scroll-up-command)
(global-set-key [mouse-9] 'scroll-down-command)
(global-set-key [mouse-10] 'list-buffers)

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

;; Theme
(use-package dracula-theme
  :init
  (load-theme 'dracula t))

;; Projectile
(use-package projectile
  :config
  (projectile-mode t))

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

;; Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Org mode
(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c L" . org-insert-link-global)
  :config
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
  (require 'org-tempo))

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

;; Other org settings
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (gnuplot . t)
     (php . t))))
(with-eval-after-load 'org
  (org-defkey org-mode-map [(meta return)] 'org-meta-return))
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar #'(lambda (file)
              (when (file-exists-p file)
                (push file org-agenda-files)))
          (org-projectile-todo-files)))
(setq org-ref-bibliography-notes "~/org/bibtex/notes.org"
      org-ref-default-bibliography '("~/org/bibtex/library.bib")
      org-ref-pdf-directory "~/org/bibtex/bibtex-pdfs")

;; Programming
;; LSP
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
(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle))))

;; Snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets :after yasnippet)
  (yas-global-mode)
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")))

;; Magit
(use-package magit
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window))))

;; Smartparens
(use-package smartparens
  :hook (prog-mode .smartparens-mode)
  :diminish smartparens-mode
  :config
  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "[" nil :actions nil))
(show-paren-mode t)

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

;; Syntax checking
(use-package flycheck
  :defer t
  :diminish
  :hook (after-init .global-flycheck-mode)
  :commands (flycheck-add-mode)
  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))
  :init
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-position 'window-bottom-left-corner)
        (flycheck-posframe-border-width 3)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode))
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (when (executable-find "vale")
    (use-package flycheck-vale
      :config
      (flycheck-vale-setup)
      (flycheck-add-mode 'vale 'latex-mode))))

;; Languages
;; ESS
(require 'ess)
(setq ess-directory "~/R/")
(ess-toggle-S-assign nil)
(load "ess-autoloads")

;; LaTeX
(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                               TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t)))
  :config
  (add-hook LaTeX-mode-hook #'display-line-numbers-mode))

;; Python
(use-package python-mode
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :custom
  (lsp-pyright-multi-root nil))

;; Web-mode
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\'" ".php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

;; PHP
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))
