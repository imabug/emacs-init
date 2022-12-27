;;; init.el -- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Some path settings
(add-to-list 'load-path "~/.config/emacs/elisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Set up user info
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com"
      user-login-name "eugenem")
(setenv "SHELL" "/bin/fish")

;; Start emacs frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Some general settings
(setq inhibit-startup-screen t)            ;Don't show the startup screen
(tool-bar-mode -1)                         ;Don't show the tool bar
(menu-bar-mode -1)                         ;Don't show the menu bar
(setq calendar-week-start-day 1)           ;Calendar week starts Monday
;; Show the time in the modeline
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode 1)                     ;Show column numbers
(show-paren-mode 1)
(recentf-mode 1)                           ;Remember recently edited files
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
;; Set tab behaviour
(setq-default tab-always-indent t
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t
              use-short-answers t)
(prefer-coding-system 'utf-8)
(global-display-line-numbers-mode t)       ;Display line numbers
(global-auto-revert-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set up some fonts
(defvar em/default-font-size 120)
(defvar em/default-variable-font-size 120)
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
(setq auto-save-timeout 120)               ;Autosave every 2 minutes
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list" t)))

;; History settings
(setq savehist-file "~/.config/emacs/savehist")
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
(savehist-mode 1)

;; Set a theme
(setq tron-legacy-theme-vivid-cursor t)
(load-theme 'tron-legacy t)

;; Mouse button bindings
(global-set-key [mouse-2] 'mark-whole-buffer)    ; wheel button
(global-set-key [mouse-6] 'backward-word)        ; thumb wheel up
(global-set-key [mouse-7] 'forward-word)         ; thumb wheel down
(global-set-key [mouse-8] 'scroll-up-command)    ; forward thumb button
(global-set-key [mouse-9] 'scroll-down-command)  ; back thumb button
(global-set-key [mouse-10] 'list-buffers)

;; Bootstrap code for straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq load-prefer-newer t
      straight-use-package-by-default t
      use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)
(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
(use-package auto-package-update
  :straight t
  :if (not(daemonp))
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
(use-package diminish
  :straight t)

(require 'iedit)
(defun iedit-dwim (arg)
  "Start iedit but use \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(global-set-key (kbd "C-;") 'iedit-dwim)

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :demand t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(rainbow-delimiters-mode t)

;;Smartparens
(use-package smartparens
  :straight t
  :demand t
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
;;; Some config borrowed from https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;;; lisp modes
(require 'smartparens-config)
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(em/add-space-before-sexp-insertion)
                 :post-handlers '(em/add-space-after-sexp-insertion)))

(defun em/add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun em/add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; Smartparens PHP
(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (em/php-handle-docstring "RET")))
  (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET") em/php-wrap-handler))
  (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

(defun em/php-wrap-handler (&rest _ignored)
  (save-excursion
    (sp-get sp-last-wrapped-region
      (goto-char :beg-in)
      (unless (looking-at "[ \t]*$")
        (newline-and-indent))
      (goto-char :end-in)
      (beginning-of-line)
      (unless (looking-at "[ \t]*}[ \t]*$")
        (goto-char :end-in)
        (newline-and-indent))
      (indent-region :beg-prf :end-suf))))

(defun em/php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ;; variable
     ((string-match (rx (or "private" "protected" "public" "var") (1+ " ") (group "$" (1+ alnum))) line)
      (let ((var-name (match-string 1 line))
            (type ""))
        ;; try to guess the type from the constructor
        (-when-let (constructor-args (my-php-get-function-args "__construct" t))
          (setq type (or (cdr (assoc var-name constructor-args)) "")))
        (insert "* @var " type)
        (save-excursion
          (insert "\n"))))
     ((string-match-p "function" line)
      (save-excursion
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args nil t))))
          (--each args
            (when (my-php-should-insert-type-annotation (cdr it))
              (insert (format "* @param %s%s\n"
                              (my-php-translate-type-annotation (cdr it))
                              (car it))))))
        (let ((return-type (save-excursion
                             (forward-line)
                             (my-php-get-function-return-type))))
          (when (my-php-should-insert-type-annotation return-type)
            (insert (format "* @return %s\n" (my-php-translate-type-annotation return-type))))))
      (re-search-forward (rx "@" (or "param" "return") " ") nil t))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))
;;Enable strict mode globally
(smartparens-global-strict-mode 1)

;; Which-key
(use-package which-key
  :straight t
  :demand t
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; Projectile
(use-package projectile
  :straight t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init
  (setq projectile-mode-line-function '(lambda () (format "[%s]" (projectile-project-name)))
        projectile-sort-order 'recentf
        projectile-enable-caching t)
  :config
  (projectile-mode +1))

;; Magit
(require 'magit)
(setq magit-credential-cache-daemon-socket nil
      magit-refresh-status-buffer nil
      magit-auto-revert-mode t
      magit-define-global-key-bindings t)

;; Company
(straight-use-package '(company :type built-in))
(setq company-backends '(company-capf
                         company-dabbrev-code
                         company-keywords)
      company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                          company-preview-frontend
                          company-echo-metadata-frontend)
      company-format-margin-function 'company-vscode-dark-icons-margin
      company-global-modes '(not shell-mode eaf-mode)
      company-idle-delay 0.1
      company-minimum-prefix-length 3
      company-show-numbers t
      company-tooltip-align-annotations t
      company-require-match nil
      company-selection-wrap-around t)
(add-hook 'after-init-hook 'global-company-mode)

;; Helm
(use-package helm
  :straight t
  :demand t
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-h i" . helm-info)
   ("C-h a" . helm-apropos)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini))
  :config
  (setq helm-actions-inherit-frame-settings       t
        helm-always-two-windows                   t
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
        helm-input-idle-delay                     0.01
        helm-move-to-line-cycle-in-source         t
        helm-recentf-fuzzy-match                  t
        helm-reuse-last-window-split-state        t
        helm-show-action-window-other-window      'left
        helm-split-window-inside-p                nil
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-no-suitable-window    t
        helm-visible-mark-prefix                  "✓"))
(use-package helm-org
  :straight t
  :demand t
  :config
  (setq helm-org-headings-fontify t))
(use-package helm-projectile
  :straight t
  :demand t
  :after projectile)
(helm-mode 1)
(helm-projectile-on)
(setq projectile-completion-system 'helm
      projectile-switch-project-action 'helm-projectile)

;; Flycheck
(use-package flycheck
  :straight t
  :demand t
  :config
  (setq flycheck-idle-change-delay 1
        flycheck-error-list-minimum-level 'warning)
  :init (global-flycheck-mode))
(use-package flycheck-color-mode-line
  :straight t)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Org mode
(straight-use-package '(org :type built-in))
(setq org-directory "~/org/"
      org-agenda-files (list "~/org/todo.org")
      org-default-notes-file "~/org/notes.org"
      org-archive-location "~/org/archive/"
      org-enable-github-support t
      org-enable-journal-support t
      org-log-done 'time-date
      org-startup-truncated nil
      org-use-speed-commands t
      org-return-follows-link t
      org-tag-alist '(("WORK" . ?W)
                      ("home" . ?h)
                      ("lab" . ?l)
                      ("research" . ?r)
                      ("dogs" . ?d)
                      ("radioclub" . ?C))
      org-capture-templates '(("t" "Todo"
                               entry (file+headline "todo.org" "Tasks")
                               "** TODO %?\n %i\n %a")
                              ("a" "Appointment"
                               entry (file+headline "todo.org" "Calendar")
                               "** APPT %^{Description} %^g\n %?\n Added: %U")
                              ("j" "Journal entry"
                               plain (function org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)
                              ("n" "Notes"
                               entry (file+olp+datetree "notes.org")
                               "* %^{Description} %^g %?\n Added: %U")
                              ("s" "Scractchpad"
                               entry (file+olp+datetree "scratchpad.org" "Scratchpad")
                               "** %^{Description} %^g %?\n Added: %U")
                              ("l" "Lab book"
                               entry (file+olp+datetree "PhD/notes.org")
                               "* %U\n %?\n %i\n %a")))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook #'turn-on-font-lock)

;; Org journal
(use-package org-journal
  :straight t
  :demand t
  :init
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-type 'weekly
        org-journal-enable-agenda-integration t))
;; Org capture templates
(defun org-journal-find-location ()
  "Open today's journal, but specify a non-nil prefix argument in order to inhibit inserting the heading; 'org-capture' will insert the heading."
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;; Languages
;; Markdown mode
(use-package markdown-mode
  :straight t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-header-scaling t
        markdown-gfm-uppercase-checkbox t))
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; PHP
(use-package php-mode
  :straight t
  :config
  (setq php-mode-coding-style 'psr2))
(use-package ac-php
    :straight t)
(use-package company-php
  :straight t
  :after (php-mode company))
(use-package flycheck-phpstan
  :straight t
  :config
  (setq phpstan-level 6))
(defun em/php-mode-setup ()
  "My PHP-mode hook."
  (require 'flycheck-phpstan)
  (flycheck-mode t))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
(add-hook 'php-mode-hook #'php-align-setup)
(add-hook 'php-mode-hook 'em/php-mode-setup)
(add-hook 'php-mode-hook
          #'(lambda ()
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

;; Web-mode
(use-package web-mode
  :straight t
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
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t
      TeX-master nil)
;; (use-package tex
;;   :ensure auctex
;;   :config
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         TeX-PDF-mode t
;;         TeX-master nil))
(use-package reftex
  :straight t
  :config
  (setq reftex-plug-into-AUCTeX t))
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook #'company-mode)

(use-package company-auctex
  :straight t
  :config
  (company-auctex-init))

(use-package company-math
  :straight t)
(with-eval-after-load 'company-math
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-latex-commands))

;; ESS
(use-package ess
  :straight t
  :config
  (setq ess-ask-for-ess-directory nil))
(require 'ess-site)

;; Define a minor mode to hold some of my keybindings
(define-minor-mode em-keymaps-mode
  "Personal keybindings."
  :init-value nil
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; Emacs frame-related keybindings
            (define-key map (kbd "C-c C-f d") 'delete-frame)
            (define-key map (kbd "C-c C-f n") 'make-frame)
            (define-key map (kbd "C-c C-f o") 'other-frame)
            (define-key map (kbd "C-c C-f l") 'lower-frame)
            (define-key map (kbd "C-c C-f r") 'raise-frame)
            ;; Org-mode keybindings
            (define-key map (kbd "C-c l") #'org-store-link)
            (define-key map (kbd "C-c a") #'org-agenda)
            (define-key map (kbd "C-c c") #'org-capture)
            (define-key map (kbd "C-c .") #'org-time-stamp)
            (define-key map (kbd "C-c ,") #'org-time-stamp-inactive)
            ;; Magit keybindings
            (define-key map (kbd "C-c C-g s") 'magit-status)
            (define-key map (kbd "C-c C-g d") 'magit-diff)
            (define-key map (kbd "C-c C-g S") 'magit-stage)
            (define-key map (kbd "C-c C-g u") 'magit-unstage)
            (define-key map (kbd "C-c C-g c") 'magit-commit)
            (define-key map (kbd "C-c C-g p") 'magit-push)
            (define-key map (kbd "C-c C-g P") 'magit-pull)
            (define-key map (kbd "C-c C-g f") 'magit-fetch)
            ;; Other keybindings
            (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
            (define-key map (kbd "<escape>") 'keyboard-escape-quit)
            (define-key map (kbd "C-c C-a") 'mark-whole-buffer)
            map))
(add-hook 'after-init-hook 'em-keymaps-mode)
(em-keymaps-mode 1)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-math company-auctex web-mode company-php ac-php php-mode which-key smartparens rainbow-delimiters helm-org helm company magit org-journal diminish auto-package-update auto-compile use-package))
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-structure ((t (:background "#1B324B" :foreground "#BBF0EF" :box nil :weight normal))))
 '(org-done ((t (:foreground "#BBF0EF" :box nil :weight bold))))
 '(org-todo ((t (:foreground "#FF7DBB" :box nil :weight bold)))))
