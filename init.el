;;; init.el  -- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Path settings
(add-to-list 'load-path "~/.config/emacs/elisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; User info
(setq user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com"
      user-login-name "eugenem")
(setenv "SHELL" "/bin/fish")

;; Start emacs frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; General settings
(setq inhibit-startup-screen t)                 ; Don't show the startup screen
(tool-bar-mode -1)                              ; Don't show the tool bar
(menu-bar-mode -1)                              ; Don't show the menu bar
(setq visible-bell t)                           ; Enable visible bell
(setq calendar-week-start-day 1)                ; Calendar week starts Monday
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)    ; Display time in the modeline
(display-time-mode)
(column-number-mode)                            ; Show column numbers
(show-paren-mode)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t)
(electric-pair-mode)
(recentf-mode)                                  ; Remember recently edited files
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default tab-always-indent t
	          indent-tabs-mode nil
	          tab-width 4)                      ; Set tab behaviour
(setq-default require-final-newline t
              use-short-answers t)
(prefer-coding-system 'utf-8)
(global-auto-revert-mode)
(global-display-line-numbers-mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		        term-mode-hook
		        shell-mode-hook
		        treemacs-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fonts
(defvar em/default-font-size 110)
(defvar em/default-variable-font-size 110)
(add-to-list 'default-frame-alist '(font . "Fira Code"))
(set-face-attribute 'default nil
		            :font "Fira Code"
		            :height em/default-font-size)
(set-face-attribute 'fixed-pitch nil
		            :font "Fira Code"
		            :height em/default-font-size)
(set-face-attribute 'variable-pitch nil
	                :font "Cantarell"
	                :height em/default-variable-font-size
	                :weight 'regular)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.config/emacs/backups"))
      delete-old-versions t
      version-control t
      vc-make-backup-files t
      auto-save-timeout 120
      auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list" t)))

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
;;(put 'helm-M-x-input-history                'history-length 50)
(put 'minibuffer-history                    'history-length 50)
(put 'kill-ring                             'history-length 50)
(savehist-mode)

;; Set a theme
(load-theme 'tron-legacy t)
(setq tron-legacy-theme-vivid-cursor t)

;; Mouse button bindings
(global-set-key [mouse-2] 'mark-whole-buffer)    ; wheel button
(global-set-key [mouse-6] 'backward-word)        ; thumb wheel up
(global-set-key [mouse-7] 'forward-word)         ; thumb wheel down
(global-set-key [mouse-8] 'scroll-up-command)    ; forward thumb button
(global-set-key [mouse-9] 'scroll-down-command)  ; back thumb button
(global-set-key [mouse-10] 'list-buffers)

;; Bootstrap code for straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
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
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package auto-compile
  :straight t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
(use-package diminish
  :straight t)

;; Set up for utility packages
(use-package ligature
  :straight t
  :demand t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                ;; =:= =!=
                ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                ;; ;; ;;;
                (";" (rx (+ ";")))
                ;; && &&&
                ("&" (rx (+ "&")))
                ;; !! !!! !. !: !!. != !== !~
                ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                ;; ?? ??? ?:  ?=  ?.
                ("?" (rx (or ":" "=" "\." (+ "?"))))
                ;; %% %%%
                ("%" (rx (+ "%")))
                ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                ;; |->>-||-<<-| |- |== ||=||
                ;; |==>>==<<==<=>==//==/=!==:===>
                ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
                ;; \\ \\\ \/
                ("\\" (rx (or "/" (+ "\\"))))
                ;; ++ +++ ++++ +>
                ("+" (rx (or ">" (+ "+"))))
                ;; :: ::: :::: :> :< := :// ::=
                (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
                ;; .. ... .... .= .- .? ..= ..<
                ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                ;; *> */ *)  ** *** ****
                ("*" (rx (or ">" "/" ")" (+ "*"))))
                ;; www wwww
                ("w" (rx (+ "w")))
                ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                ;; << <<< <<<<
                ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
                ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                ;; >> >>> >>>>
                (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
                ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                ;; __ ___ ____ _|_ __|____|_
                ("_" (rx (+ (or "_" "|"))))
                ;; Fira code: 0xFF 0x12
                ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                ;; Fira code:
                "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                ;; The few not covered by the regexps.
                "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  :hook (prog-mode . global-ligature-mode))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :demand t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-key
(use-package which-key
  :straight t
  :demand t
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; Company for completions
(use-package company
  :straight (:type built-in)
  :config
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
  :hook (after-init . global-company-mode))

;; Magit
(use-package magit
  :straight (:type built-in)
  :config
  (setq magit-credential-cache-daemon-socket nil
        magit-refresh-status-buffer nil
        magit-auto-revert-mode t
        magit-define-global-key-bindings t))

;; Flycheck
(use-package flycheck
  :straight (:type built-in)
  :ensure t
  :config
  (setq flycheck-idle-change-delay 1
        flycheck-error-list-minimum-level 'warning)
  :init (global-flycheck-mode)
  :hook (after-init . global-flycheck-mode))

;; Org mode
(use-package org
  :straight t
  :defer t
  :config
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
  :hook (org-mode . #'turn-on-font-lock)
  :mode ("\\.org\\'"))

;; Programming modes
;; PHP
(use-package php-mode
  :straight t
  :defer t
  :config (setq php-mode-coding-style 'psr2)
  :mode ("\\.php\\'"))
(use-package company-php
  :straight t
  :defer t
  :after (php-mode company))
(use-package ac-php
  :straight t
  :defer t
  :after (php-mode company))
(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook
          #'(lambda ()
              ;; Enable ElDoc support (optional)
              (ac-php-core-eldoc-setup)

              (set (make-local-variable 'company-backends)
                   '((company-ac-php-backend company-dabbrev-code)
                     company-capf company-files))

              ;; Jump to definition (optional)
              (define-key php-mode-map (kbd "M-]")
                          'ac-php-find-symbol-at-point)

              ;; Return back (optional)
              (define-key php-mode-map (kbd "M-[")
                          'ac-php-location-stack-back)))
  (define-key php-mode-map (kbd "C-c C--") 'php-current-class)
  (define-key php-mode-map (kbd "C-c C-=") 'php-current-namespace))

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
