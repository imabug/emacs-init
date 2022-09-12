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
(setenv "SHELL" "/bin/fish")

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
(recentf-mode 1)                           ; Remember recently edited files
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
(load-theme 'tron-legacy t)
(setq tron-legacy-theme-vivid-cursor t)

;; Mouse button bindings
(global-set-key [mouse-6] 'backward-word)
(global-set-key [mouse-7] 'forward-word)
(global-set-key [mouse-8] 'scroll-up-command)
(global-set-key [mouse-9] 'scroll-down-command)
(global-set-key [mouse-10] 'list-buffers)

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
;; Org tags
(setq org-tag-alist '(("WORK" . ?W)
                      ("home" . ?h)
                      ("lab" . ?l)
                      ("research" . ?r)
                      ("dogs" . ?d)
                      ("radioclub" . ?C)))
;; Org journal
(use-package org-journal
  :ensure t
  :defer t
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
(setq org-capture-templates '(("t" "Todo"
                               entry (file+headline "~/org/todo.org" "Tasks")
                               "** TODO %?\n %i\n %a")
                              ("a" "Appointment"
                               entry (file+headline "~/org/todo.org" "Calendar")
                               "** APPT %^{Description} %^g\n %?\n Added: %U")
                              ("j" "Journal entry"
                               plain (funnction org-journal-find-location)
                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                               :jump-to-captured t :immediate-finish t)
                              ("n" "Notes"
                               entry (file+olp+datetree "~/org/notes.org")
                               "* %^{Description} %^g %?\n Added: %U")
                              ("l" "Lab book"
                               entry (file+olp+datetree "~/org/PhD/notes.org")
                               "* %U\n %?\n %i\n %a")))

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

;; Company mode
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match nil)
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;Smartparens
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
;;Enable strict mode globally
(smartparens-global-strict-mode 1)

;; Which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

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
;;(require 'tex-site)
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-master nil))
(use-package reftex)
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

;; Define a minor mode to hold some of my keybindings
(define-minor-mode em-keymaps-mode
  "Personal keybindings"
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
            ;; Other keybindings
            (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
            (define-key map (kbd "<escape>") 'keyboard-escape-quit)
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
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(org-agenda-structure ((t (:background "#1B324B" :foreground "#BBF0EF" :box nil :weight normal))))
 '(org-done ((t (:foreground "#BBF0EF" :box nil :weight bold))))
 '(org-todo ((t (:foreground "#FF7DBB" :box nil :weight bold)))))
