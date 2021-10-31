;;; init.el --- -*- lexical-binding: t -*-

;;; Code:
;; Some path settings
(add-to-list 'load-path "~/.config/emacs/elisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
;; (setq custom-file "~/.config/emacs/custom-settings.el")

;; Set up user info
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com"
      calendar-latitude 33.0752523
      calendar-longitude -80.0220569)
(setenv "SHELL" "/opt/bin/fish")

;; Start up maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Some general settings
;; Don't show the startup screen.
(setq inhibit-startup-screen t)
;; Don't show the tool bar or menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Show the time in the modeline
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)
;; Show column numbers
(column-number-mode 1)
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
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Display line numbers
(global-display-line-numbers-mode t)
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
;; Default font
(set-face-attribute 'default nil :font "Hack" :height em/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack" :height em/default-font-size)
;; Variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height em/default-variable-font-size :weight 'regular)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq delete-old-versions t)
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
(put 'minibuffer-history                    'history-length 50)
(put 'kill-ring                             'history-length 50)

;; Keybindings
;; Extra mouse button/wheel bindings for my Logitech MX Master mouse
(global-set-key [mouse-6] 'backward-word)
(global-set-key [mouse-7] 'forward-word)
(global-set-key [mouse-8] 'scroll-up-command)
(global-set-key [mouse-9] 'scroll-down-command)
(global-set-key [mouse-10] 'list-buffers)
;; Set some frame related keybindings
(global-set-key (kbd "C-c C-f d") 'delete-frame)
(global-set-key (kbd "C-c C-f n") 'make-frame)
(global-set-key (kbd "C-c C-f o") 'other-frame)
(global-set-key (kbd "C-c C-f s") 'select-frame)
(global-set-key (kbd "C-c C-f l") 'lower-frame)
(global-set-key (kbd "C-c C-f r") 'raise-frame)
;; Other keybindings
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;; Set a theme
(load-theme 'tron-legacy t)
(setq tron-legacy-theme-vivid-cursor t)

;; Package management setup
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's not installed already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
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
  (company-selection-wrap-around t)
  (company-text-face-extra-attributes '(:weight bold))
  :config
  (global-company-mode 1))

;; Helm
(use-package helm
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-h i" . helm-info)
   ("C-h a" . helm-apropos)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini))
  :config
  (require 'helm-config)
  (setq helm-input-idle-delay                     0.01
        helm-always-two-windows                   t
        helm-actions-inherit-frame-settings       t
        helm-allow-mouse                          t
        helm-autoresize-max-height                0 ; it is %.
        helm-autoresize-min-height                10 ; it is %.
        helm-autoresize-mode                      t
        helm-buffers-fuzzy-matching               t
        helm-candidate-number-limit               500
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-display-buffer-default-height        10 ;; Make the helm buffer smaller
        helm-follow-mode-persistent               t
        helm-frame-background-color               "DarkSlateGray"
        helm-move-to-line-cycle-in-source         t
        helm-recentf-fuzzy-match                  t
        helm-reuse-last-window-split-state        t
        helm-show-action-window-other-window      'left
        helm-split-window-inside-p                nil
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-no-suitable-window    t
        helm-visible-mark-prefix                  "✓"))
(use-package helm-org
  :config
  (setq helm-org-headings-fontify t))
(helm-mode 1)

;; Org mode
;; Use the version of org-mode that comes bundled with Fedora's emacs
;; instead of grabbing it from ELPA.
;; It will be a bit older, but then I don't need to worry about conflicts
;; because of loading different versions
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'turn-on-font-lock)
(setq org-agenda-files (list "~/org/todo.org")
      org-default-notes-file "~/org/notes.org"
      org-directory "~/org/"
      org-enable-github-support t
      org-enable-journal-support t
      org-log-done 'time-date      
      org-startup-truncated nil
      org-use-speed-commands t)
(setq org-ref-bibliography-notes "~/org/bibtex/notes.org"
      org-ref-default-bibliography '("~/org/bibtex/library.bib")
      org-ref-pdf-directory "~/org/bibtex/bibtex-pdfs")

;; Org-mode key bindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Org tags
(setq org-tag-alist '(("WORK" . ?W)
                      ("home" . ?h)
                      ("lab" . ?l)
                      ("research" . ?r)
                      ("dogs" . ?d)
                      ("radioclub" . ?C)))

;; Org journal
(use-package org-journal
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-enable-agenda-integration t))
(with-eval-after-load 'org
  (require 'org-tempo)
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("php" . "src php"))
  ;; Change the face attributes for some org items
  (set-face-attribute 'org-done nil :weight 'bold :box nil :foreground "#BBF0EF")
  (set-face-attribute 'org-todo nil :weight 'bold :box nil :foreground "#FF7DBB")
  (set-face-attribute 'org-agenda-structure nil
                      :weight 'bold :box nil :foreground "#BBF0EF" :background "#1B324B")
  (set-face-attribute 'org-table-header nil
                      :inherit 'org-table :weight 'bold :background "LightGray" :foreground "Black"))

;; Org capture templates
(defun org-journal-find-location ()
  "Open today's journal, but specify a non-nil prefix argument in order to inhibit inserting the heading; 'org-capture' will insert the heading."
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))
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
                               "* %U\n %?\n %i\n %a")
                              ("j" "Journal entry" plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)))

;; Magit
(use-package magit
  :ensure t
  :demand t
  :bind
  (("C-c C-g s" . magit-status)
   ("C-c C-g d" . magit-diff)
   ("C-c C-g S" . magit-stage)
   ("C-c C-g u" . magit-unstage)
   ("C-c C-g c" . magit-commit)
   ("C-c C-g p" . magit-push)
   ("C-c C-g P" . magit-pull)
   ("C-c C-g f" . magit-fetch))
  :config
  (setq magit-credential-cache-daemon-socket nil)
  (magit-auto-revert-mode))

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode 1)
  :diminish projectile-mode
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/workspace")
        projectile-find-dir-includes-top-level t
        projectile-enable-caching t
        projectile-switch-project-action #'projectile-commander
        projectile-sort-order 'recently-active
        projectile-completion-system 'helm)
  (projectile-register-project-type 'php '("composer.json")
                                    :src-dir "app"
				                    :test "composer test"
				                    :run "composer serve"
				                    :test-suffix "Test"
				                    :test-dir "tests"))
(use-package org-projectile
  :ensure t
  :after (org org-agenda projectile)
  :bind (("C-c n p" . org-projectile-project-todo-completing-read))
  :config
  (progn
    (setq org-projectile-projects-file "~/org/todo.org"
          org-projectile-file "~/org/todo.org"
          org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))
(use-package helm-projectile
  :after (helm projectile))
(helm-projectile-on)

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-)" . sp-forward-slurp-sexp)
        ("C-}" . sp-forward-barf-sexp)
        ("C-(" . sp-backward-slurp-sexp)
        ("C-{" . sp-backward-barf-sexp))
  :config
  (progn
    ;; Stop pairing single quotes in elisp
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'org-mode "[" nil :actions nil)
    (sp-local-pair 'org-mode "~" "~")))
;; Enable strict mode globally
(smartparens-global-strict-mode 1)

;; Rainbow delimiters
(use-package rainbow-delimiters
   :hook (prog-mode . rainbow-delimiters-mode))

;; Flycheck
;; Use Fedora's emacs-flycheck package
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(diminish flycheck-mode)

;; Winum
(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-`") 'winum-select-window-by-number)
      (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
      (define-key map (kbd "M-1") 'winum-select-window-1)
      (define-key map (kbd "M-2") 'winum-select-window-2)
      (define-key map (kbd "M-3") 'winum-select-window-3)
      (define-key map (kbd "M-4") 'winum-select-window-4)
      (define-key map (kbd "M-5") 'winum-select-window-5)
      (define-key map (kbd "M-6") 'winum-select-window-6)
      (define-key map (kbd "M-7") 'winum-select-window-7)
      (define-key map (kbd "M-8") 'winum-select-window-8)
      map))
(use-package winum)
(winum-mode)

;; Which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
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
  (treemacs-no-delete-other-windows t)
  (treemacs-no-png-images nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-project-follow-cleanup t)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch t)
  (treemacs-silent-refresh t)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-width 20)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
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
  :ensure t
  :after (treemacs magit))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :after (treemacs projectile))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t
  :defer t)

;; Doom modeline
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-irc nil
        doom-modeline-gnus nil
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-file-name-style 'relative-to-project))

;; ;; LSP Language Server Protocol
;; (use-package lsp-mode
;;   :bind
;;   (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable t
;;         lsp-prefer-flymake nil
;;         lsp-keymap-prefix "C-x l"
;;         gc-cons-threshold (* 100 1024 1024)
;;         read-process-output-max (* 1024 1024)
;;         company-idle-delay 0.0
;;         company-minimum-prefix-length 1
;;         ;; lock files will kill `npm start'
;;         create-lockfiles nil)
;;   :hook
;;   ((java-mode python-mode go-mode rust-mode
;;               js-mode js2-mode typescript-mode web-mode
;;               c-mode c++-mode objc-mode) . lsp-deferred))
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;     (lsp-ui-doc-background ((t (:background nil))))
;;     (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;     :bind
;;     (:map lsp-ui-mode-map
;;           ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;           ([remap xref-find-references] . lsp-ui-peek-find-references)
;;           ("C-c u" . lsp-ui-imenu)
;;           ("M-i" . lsp-ui-doc-focus-frame))
;;     (:map lsp-mode-map
;;           ("M-n" . forward-paragraph)
;;           ("M-p" . backward-paragraph))
;;     :custom
;;     (lsp-ui-doc-header t)
;;     (lsp-ui-doc-include-signature t)
;;     (lsp-ui-doc-border (face-foreground 'default))
;;     (lsp-ui-sideline-enable nil)
;;     (lsp-ui-sideline-ignore-duplicate t)
;;     (lsp-ui-sideline-show-code-actions nil)
;;     :config
;;     ;; Use lsp-ui-doc-webkit only in GUI
;;     (when (display-graphic-p)
;;       (setq lsp-ui-doc-use-webkit t))
;;     ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;     ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;     (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;       (setq mode-line-format nil))
;;     ;; `C-g'to close doc
;;     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))
;; ;; (use-package dap-mode
;; ;;   :diminish
;; ;;   :bind
;; ;;   (:map dap-mode-map
;; ;;         (("<f12>" . dap-debug)
;; ;;          ("<f8>" . dap-continue)
;; ;;          ("<f9>" . dap-next)
;; ;;          ("<M-f11>" . dap-step-in)
;; ;;          ("C-M-<f11>" . dap-step-out)
;; ;;          ("<f7>" . dap-breakpoint-toggle))))

;; Languages
;; PHP
(use-package php-mode
  :ensure t
  :config
  (setq php-mode-coding-style 'psr2))
(use-package ac-php)
(use-package company-php
  :defer
  :after (php-mode company))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(setq php-mode-coding-style 'psr2)
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
(add-hook 'php-mode-hook #'php-align-setup)
(add-hook 'php-mode-hook
          '(lambda ()
             (company-mode t)
             (require 'company-php)
             (set (make-local-variable 'company-backends)
                  '((company-ac-php-backend company-dabbrev-code)
                    company-capf company-files))
             (define-key php-mode-map (kbd "M-]")
               'ac-php-find-symbol-at-point)
             (define-key php-mode-map (kbd "M-[")
               'ac-php-location-stack-back)))
(with-eval-after-load 'php-mode
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))

;; ;; Web-mode
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"
   "\\.html?\\'" "\\.blade\\.php\\'")
  :config
  (setq web-mode-engines-alist
        '(("php" . "\\.phtml\\'")
          ("blade" . "\\.blade\\."))
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t
        web-mode-enable-engine-detection t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; LaTeX
(require 'tex-site)
(require 'reftex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(use-package company-auctex
  :defer t
  :config
  (company-auctex-init))

(use-package company-math
  :defer t)
(with-eval-after-load 'company-math
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-latex-commands))

;; Load custom settings from custom-file (custom-settings.el)
;; (add-hook 'after-init-hook (lambda () (load custom-file)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters org-journal company-math selectric-mode winum which-key web-mode use-package treemacs-projectile treemacs-magit treemacs-icons-dired smartparens org-projectile lsp-ui lsp-treemacs helm-projectile helm-org flycheck ess doom-modeline diminish company-php company-auctex bui auto-package-update auto-compile ac-php))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(font-lock-comment-face ((t (:foreground "#828282")))))
