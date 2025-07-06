;;; init.el  -- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; User info
(setopt user-full-name "Eugene Mah"
      user-mail-address "eugenemah@gmail.com"
      user-login-name "eugenem")
(setenv "SHELL" "/bin/fish")

;; General settings
(setopt calendar-week-start-day 1)                ; Calendar week starts Monday
(setopt display-time-day-and-date t
        display-time-24hr-format t
        display-time-default-load-average nil)
;; Electric-pair options
(setopt electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t)
(setopt initial-major-mode 'text-mode)
;; Set tab behaviour
(setopt tab-always-indent t
	    indent-tabs-mode nil
	    tab-width 4)
(setopt require-final-newline t
        use-short-answers t)
;; Abbrev options
(setopt abbrev-suggest t
        abbrev-file-name "~/.config/emacs/abbrev_defs") ; Set abbrevs file name
;; Activate modes
(electric-pair-mode)                            ; Enable electric-pair mode
(display-time-mode)                             ; Display time in the mode line
(abbrev-mode)                                   ; Enable abbrev mode
(column-number-mode)                            ; Show column numbers
(show-paren-mode)                               ; Highlight matching parens
(recentf-mode)                                  ; Remember recently edited files
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(prefer-coding-system 'utf-8)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

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
(setopt backup-directory-alist '(("." . "~/.config/emacs/backups"))
        delete-old-versions t
        version-control t
        vc-make-backup-files t
        auto-save-timeout 120
        auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list" t)))

;; History settings
(setopt savehist-file "~/.config/emacs/savehist")
(setopt history-delete-duplicates t
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

;; Mouse button bindings
(keymap-global-set "<mouse-2>" 'mark-whole-buffer)    ; wheel button
(keymap-global-set "<mouse-6>" 'backward-word)        ; thumb wheel up
(keymap-global-set "<mouse-7>" 'forward-word)         ; thumb wheel down
(keymap-global-set "<mouse-8>" 'scroll-up-command)    ; forward thumb button
(keymap-global-set "<mouse-9>" 'scroll-down-command)  ; back thumb button
(keymap-global-set "<mouse-10>" 'list-buffers)

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
;;(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Delete selected text upon insertion
(use-package delsel
  :straight (:type built-in)
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Use doom modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-time-live-icon nil)
  (doom-modeline-column-zero-based nil)
  :hook (after-init . doom-modeline-mode))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Set up for utility packages
(use-package ligature
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
  :demand t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-key
(use-package which-key
  :straight (:type built-in)
  :demand t
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; Helm
(use-package helm
  :custom
  (helm-apropos-fuzzy-match t)
  (helm-autoresize-mode t)
  (helm-display-buffer-default-height 20)
  (helm-lisp-fuzzy-completion t)
  (helm-locate-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  (helm-M-x-show-short-doc t)
  (helm-move-to-line-cycle-in-source t)
  (helm-scroll-amount 10)
  (helm-split-window-default-side "right")
  :bind
  ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :hook (after-init . helm-mode))
;; Helm key bindings
(global-unset-key (kbd "C-x c"))

;; Company for completions
(use-package company
  :straight (:type built-in)
  :custom
  (company-backends '(company-capf
                      company-dabbrev-code
                      company-keywords
                      company-clang))
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
                       company-preview-frontend
                       company-echo-metadata-frontend))
  (company-format-margin-function 'company-vscode-dark-icons-margin)
  (company-global-modes '(not shell-mode eaf-mode))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 3)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-selection-wrap-around t)
  :bind
  (:map company-active-map
              ("<tab>" . company-complete-selection))
  :hook (after-init . global-company-mode))

;; Magit
(use-package magit
  :straight (:type built-in)
  :custom
  (magit-credential-cache-daemon-socket nil)
  (magit-refresh-status-buffer nil)
  (magit-auto-revert-mode t)
  (magit-define-global-key-bindings t)
  :bind
  ("C-c C-g s" . magit-status)
  ("C-c C-g d" . magit-diff)
  ("C-c C-g D" . magit-diff-unstaged)
  ("C-c C-g S" . magit-stage)
  ("C-c C-g U" . magit-unstage)
  ("C-c C-g c" . magit-commit)
  ("C-c C-g p" . magit-push)
  ("C-c C-g P" . magit-pull)
  ("C-c C-g f" . magit-fetch)
  ("C-c C-g l" . magit-log)
  ("C-c C-g b" . magit-branch)
  ("C-c C-g t" . magit-tag))

;; Flycheck
(use-package flycheck
  :straight (:type built-in)
  :ensure t
  :custom
  (flycheck-idle-change-delay 1)
  (flycheck-error-list-minimum-level 'warning)
  :hook (after-init . global-flycheck-mode))

;; Org mode
(use-package org
  :straight (:type built-in)
  :custom
  (org-directory "~/org/")
  (org-agenda-files (list "~/org/todo.org"))
  (org-default-notes-file "~/org/notes.org")
  (org-archive-location "~/org/archive/")
  (org-enable-github-support t)
  (org-enable-journal-support t)
  (org-log-done 'time-date)
  (org-startup-truncated nil)
  (org-use-speed-commands t)
  (org-return-follows-link t)
  (org-tag-alist '(("WORK" . ?W)
                   ("home" . ?h)
                   ("lab" . ?l)
                   ("research" . ?r)
                   ("dogs" . ?d)
                   ("radioclub" . ?C)))
  (org-capture-templates '(("t" "Todo"
                            entry (file+headline "todo.org" "Tasks")
                            "** TODO %?\n %i\n %a")
                           ("a" "Appointment"
                            entry (file+headline "todo.org" "Calendar")
                            "** APPT %^{Description} %^g\n %?\n Added: %U")
                           ("j" "Journal entry"
                            entry (file+olp+datetree "~/org/journal/journal.org")
                            "* %?\nEntered on %U\n %i\n %a")
                           ("n" "Notes"
                            entry (file+olp+datetree "notes.org")
                            "* %^{Description} %^g %?\n Added: %U")
                           ("s" "Scractchpad"
                            entry (file+olp+datetree "scratchpad.org" "Scratchpad")
                            "** %^{Description} %^g %?\n Added: %U")
                           ("l" "Lab book"
                            entry (file+olp+datetree "PhD/notes.org")
                            "* %U\n %?\n %i\n %a"))))

;; Org journal
(use-package org-journal
  :init
  (setq org-journal-prefix-key "C-c j")
  :custom
  (setq org-journal-dir "~/org/journal/"
        org-journal-enable-agenda-integration t
        org-journal-file-type 'year))

;; Programming modes
;; tree-sitter
(setq treesit-language-source-alist
      '((arduino "https://github.com/tree-sitter-grammars/tree-sitter-arduino")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setopt major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (cpp-mode . cpp-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (java-mode . java-ts-mode)
        (javascript-mode . javascript-ts-mode)
        (json-mode . json-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (php-mode . php-ts-mode)
        (python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(setq treesit-load-name-override-list '((gomod "libtree-sitter-go")))

;; ;; eglot
;; (setq eglot-autoshutdown t
;;       lsp-phpactor-path "phpactor")
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(php-mode . ("phpactor" "language-server"))))
;; (add-hook 'php-mode-hook 'eglot-ensure)

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  (markdown-command "multimarkdown"))

;; Fish shell
(use-package fish-mode
  :custom
  (fish-enable-auto-indent t))

;; PHP
(use-package php-mode
  :defer t
  :custom
  (php-mode-coding-style 'psr2)
  :mode ("\\.php\\'"))
(use-package company-php
  :defer t
  :after (php-mode company))
(use-package ac-php
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
            ;; EasyPG interface for GPG
            (define-key map (kbd "C-c M-e l") 'epa-list-keys)
            (define-key map (kbd "C-c M-e L") 'epa-list-secret-keys)
            (define-key map (kbd "C-c M-e v") 'epa-verify-region)
            (define-key map (kbd "C-c M-e V") 'epa-verify-file)
            (define-key map (kbd "C-c M-e d") 'epa-decrypt-region)
            (define-key map (kbd "C-c M-e D") 'epa-decrypt-file)
            (define-key map (kbd "C-c M-e e") 'epa-encrypt-region)
            (define-key map (kbd "C-c M-e E") 'epa-encrypt-file)
            (define-key map (kbd "C-c M-e s") 'epa-sign-region)
            (define-key map (kbd "C-c M-e S") 'epa-sign-file)
            ;; Other keybindings
            (define-key map (kbd "C-c ;") 'comment-or-uncomment-region)
            (define-key map (kbd "<escape>") 'keyboard-escape-quit)
            (define-key map (kbd "C-c C-a") 'mark-whole-buffer)
            (define-key map (kbd "C-c C-p") 'mark-paragraph)
            map))
(add-hook 'after-init-hook 'em-keymaps-mode)
(em-keymaps-mode 1)

(provide 'init)
;;; init.el ends here
