;;; init.el --- Vanilla Emcas Config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 JNSFilipe
;;
;; Author: JNSFilipe <jose.filipe@ieee.org>
;; Maintainer: JNSFilipe <jose.filipe@ieee.org>
;; Created: junho 11, 2024
;; Modified: junho 11, 2024
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Sources/Inspiration:
;; https://blog.sumtypeofway.com/posts/emacs-config.html

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "JNSFilipe"
      user-mail-address "jose.filipe@ieee.org")

;; Setting it to 100mb seems to strike a nice balance between GC pauses and performance.
(setq gc-cons-threshold (* 100 1024 1024))

;; TODO:
;; - [ ] Add way to search documentation
;; - [ ] Add copilot
;; - [x] Solve warning at the beginning -- DISABLED WARINING AT BEGINGI, NOT EXACTLY SOLVED

;; #############################################################################
;; Bootstrap elpaca
;; #############################################################################
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;; #############################################################################
;; Helper functions
;; #############################################################################
(defun vemacs/find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(defun vemacs/dired ()
  (interactive)
  (if (projectile-project-p)
      (dired (projectile-project-root))
    (dired "~/")))

(defun vemacs/xy-window-pixel-ratio ()
  "Return the ratio of the window's width to its height in pixels."
  (interactive)
  (let* ((edges (window-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (ratio (/ (float width) height)))
    (if (called-interactively-p 'interactive)
        (message "Width/Height Ratio: %f" ratio)
      ratio)))

(defun vemacs/auto-split-window ()
  "Split the current window along its biggest dimension and run `projectile-find-file`."
  (interactive)
  (if (> (vemacs/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  (other-window 1)
  (projectile-find-file))

(defun vemacs/indent-region (num-spaces)
  "Indent or unindent the selected region by NUM-SPACES."
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (indent-rigidly (point) (min (1+ end) (line-end-position)) num-spaces)
            (forward-line)))
        (setq deactivate-mark nil))
    (if (< num-spaces 0) (vemacs/forward-global-mark) (vemacs/backward-global-mark))))


(defun vemacs/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun vemacs/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))
;; #############################################################################

;; Basic Emacs setup
(use-package emacs
  ;; https://github.com/progfolio/elpaca?tab=readme-ov-file#quick-start / https://www.reddit.com/r/emacs/comments/1bgurp5/how_to_turn_off_elpacausepackagecompact_warning/
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; UI elements setup
  (dolist (mode
           '(menu-bar-mode       ;; Disable the menu bar
             tool-bar-mode       ;; Disable the tool bar
             scroll-bar-mode     ;; Disable the scroll bars
             blink-cursor-mode)) ;; Disable the blinking cursor
    (funcall mode -1))

  (setq
   ;; Support opening new minibuffers from inside existing minibuffers.
   enable-recursive-minibuffers t
   ;; No need to see GNU agitprop.
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is.
   initial-scratch-message nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Save existing clipboard text into the kill ring before replacing it.
   save-interprogram-paste-before-kill t
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; accept 'y' or 'n' instead of yes/no
   ;; the documentation advises against setting this variable
   ;; the documentation can get bent imo
   use-short-answers t
   ;; Confir kill emacs
   confirm-kill-emacs #'yes-or-no-p
   ;; eke out a little more scrolling performance
   fast-but-imprecise-scrolling t
   ;; prefer newer elisp files
   load-prefer-newer t
   ;; if native-comp is having trouble, there's not very much I can do
   native-comp-async-report-warnings-errors 'silent
   ;; unicode ellipses are better
   truncate-string-ellipsis "…"
   ;; Define Scrool step
   scroll-step 1
   ;; Smooth scrolling
   scroll-conservatively 10000
   ;; Resize at pixel resolution
   window-resize-pixelwise t
   frame-resize-pixelwise t
   ;; Store emacs generated files in a centralised location
   backup-directory-alist '(("." . "~/.emacs_saves"))
   ;; Store automatic customization options elsewhere
   custom-file (locate-user-emacs-file "custom.el"))

  ;; Never mix tabs and spaces. Never use tabs, period.
  ;; We need the setq-default here because this becomes
  ;; a buffer-local variable when set.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq indent-line-function 'insert-tab)

  ;; Set encoding preferences
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)

  ;; For navigating wrapped lines
  (global-visual-line-mode t)

  ;; Higlight line with cursor
  (hl-line-mode t)

  ;; Enable and use relative line numbering
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)

  ;; Automatically pair parentheses
  (electric-pair-mode t)

  ;; Fonts
  (set-face-attribute 'default nil :font "Cascadia Code-11")
  ;; Pretty simbols
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                         ("delta" . ?Δ)
                                         ("gamma" . ?Γ)
                                         ("phi" . ?φ)
                                         ("psi" . ?ψ)))

  ;; Miscellaneous options -- Stolen from somwhere, don't know where nor what it does...
  (setq-default major-mode
                (lambda ()
                  (unless buffer-file-name
                    (let ((buffer-file-name (buffer-name)))
                      (set-auto-mode)))))
  (save-place-mode t)
  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (savehist-mode t)
  (recentf-mode t)

  ;; Load custom.el if it exists...
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; DISABLE WARNINGS AT INITIALIZATION, REMOVE FOR DEBUG, IT IS A LAZY WORKAROUND!!!
  (setq warning-minimum-level :emergency)

  ;; Share system/emacs clipboard
  (setq x-select-enable-clipboard t))

;; Without this, flymake whines about installed version of eldoc being too low
(use-package jsonrpc)
(use-package eldoc
  ;; :defer t
  :hook (vertigo-mode . turn-on-eldoc-mode)
  :config
  (eldoc-idle-delay 0.3)
  (provide 'upgraded-eldoc))

;; Install icons
(use-package all-the-icons :if (display-graphic-p))
(use-package nerd-icons :if (display-graphic-p))

;; Ligatures
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Doom Themes
(use-package doom-themes
  :config
  ;; Load the desired theme
  (load-theme 'doom-tokyo-night t)  ; Replace 'doom-one with your preferred theme
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Doom Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal t)
  (setq doom-modeline-height 15))

;; Dims non-active windows
(use-package dimmer
  :custom (dimmer-fraction 0.3)
  :config (dimmer-mode))

;; Projectile.el stuff
(use-package projectile
  :init (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Documents/GitHub/"))
  (setq projectile-sort-order 'access-time)
  :bind
  ("C-SPC" . projectile-switch-project))

;; Create persistent session
(use-package perspective
  :init
  (persp-mode))

;; Create different perspectives per project
(use-package persp-projectile)

;; A startup screen extracted from Spacemacs
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'projectile
        dashboard-banner-logo-title nil
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-footer nil
        dashboard-page-separator "\n\n\n"
        dashboard-items '((projects . 5)
                          (recents  . 5)))
  (dashboard-setup-startup-hook))

;; Flymake - Inline static analysis
(use-package flymake
  :after eldoc
  :hook (prog-mode . flymake-mode)
  :config

  ;; Show diagnoses in minibuffer
  ;; https://github.com/joaotavora/eglot/discussions/898
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Show flymake diagnostics first.
              (setq eldoc-documentation-functions
                    (cons #'flymake-eldoc-function
                          (remove #'flymake-eldoc-function eldoc-documentation-functions)))
              ;; Show all eldoc feedback.
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))

  ;; Ensure flymake tooltips appear on the right
  (setq flymake-fringe-indicator-position 'right-fringe))
  

;; Treesitter
(use-package tree-sitter
  :hook ((python-mode . tree-sitter-hl-mode)
         (rust-mode . tree-sitter-hl-mode)
         (sh-mode . tree-sitter-hl-mode)
         (c-mode . tree-sitter-hl-mode)
         (cpp-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)
         (zig-mode . tree-sitter-hl-mode)
         (haskel-mode . tree-sitter-hl-mode)
         (ocaml-mode . tree-sitter-hl-mode)
         (latex-mode . tree-sitter-hl-mode)))
(use-package tree-sitter-langs)

;; Eglot - LSP Support
(use-package eglot
  :init (defalias 'start-lsp-server #'eglot)
  :hook ((go-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (cpp-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (haskel-mode . eglot-ensure)
         (ocaml-mode . eglot-ensure)
         (latex-mode . eglot-ensure))
  (prog-mode . eglot-ensure)
  :custom
  (eglot-autoshutdown t))
(use-package consult-eglot)

;; Auto-formatting
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci"))))
  ;; To format on save
  (format-all-mode t))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Aggressive Indent, seems useful but on probationn for now
(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; Git signs
;; https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (custom-set-variables
   '(git-gutter:modified-sign "~")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-"))
  (setq git-gutter:update-interval 0.2))

;; EditorConfig support
(use-package editorconfig
  :config (editorconfig-mode t))

;; Undo backend
(use-package undo-fu)

;; EAT - Terminal emulation
(use-package eat)

;; Enable vertico
(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Marginalia, whateve it is...
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")) ;; "C-+"

;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;; By default `consult-project-function' uses `project-root' from project.el.
;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
;; (setq consult-project-function nil)


;; Orderless
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Embark
(use-package embark

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-consult
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Autocompletion
(use-package corfu
  :init
  (global-corfu-mode)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
        corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)))

;; Evil stuff
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-respect-visual-line-mode t) ;; respect visual lines
  (setq evil-search-module 'isearch) ;; use emacs' built-in search functionality.
  (setq evil-want-C-u-scroll t)
  (evil-mode)
  :bind
  ("<escape>" . keyboard-escape-quit)
  :config
  ;; Vim remapings - do not work in :bind
  ;; NORMAL mode
  (define-key evil-normal-state-map (kbd "j") "gj")
  (define-key evil-normal-state-map (kbd "k") "gk")
  (define-key evil-normal-state-map (kbd "w") "viw")
  (define-key evil-normal-state-map (kbd "W") "viW")
  (define-key evil-normal-state-map (kbd "$") "g_") ;; https://stackoverflow.com/questions/20165596/select-entire-line-in-vim-without-the-new-line-character
  (define-key evil-normal-state-map (kbd "q") 'delete-window)
  (define-key evil-normal-state-map (kbd "C-q") 'delete-window)
  (define-key evil-normal-state-map "∇" 'consult-global-mark)
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-jump-backward)
  (define-key evil-normal-state-map (kbd "<backtab>") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "ç") 'git-gutter:next-hunk)
  (define-key evil-normal-state-map (kbd "Ç") 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  ;; VISIAL mode
  (define-key evil-visual-state-map (kbd "$") "g_") ;; https://stackoverflow.com/questions/20165596/select-entire-line-in-vim-without-the-new-line-character
  (define-key evil-visual-state-map (kbd "<tab>") 'vemacs/evil-shift-right)
  (define-key evil-visual-state-map (kbd "<backtab>") 'vemacs/evil-shift-left)
  )

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :after evil
    :config (etcc-on)))

;; Smart comments (build on top of comment-dwin)
(use-package smart-comment)

;; Highlight FIXME/TODO/BUG
(use-package fic-mode
  :config (fic-mode))

;; Key-chord, to deal with sequances of keys, like jj to escape insert mode
(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  ;; Insert mode
  (key-chord-define meow-insert-state-keymap "jj" 'evil-normal-state))

;; Which-key and general.el stuff
(use-package which-key
  :init
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-side-window-location 'bottom
	      which-key-sort-order #'which-key-key-order-alpha
	      which-key-sort-uppercase-first nil
	      which-key-add-column-padding 1
	      which-key-max-display-columns nil
	      which-key-min-display-lines 6
	      which-key-side-window-slot -10
	      which-key-side-window-max-height 0.25
	      which-key-idle-delay 0.8
	      which-key-max-description-length 25
	      which-key-allow-imprecise-window-fit t
	      which-key-separator " → " ))

(use-package general
  :after evil
  :config
  (general-evil-setup)

  (defconst my-keybindings
    ;; TODO: Add names to which-key
    '((":" . execute-extended-command)
      (";" . eval-expression)
      ("." . ibuffer)
      ("," . scratch-buffer)
      ("*" . project-search)
      ("'" . text-scale-adjust)
      ("b" . consult-buffer)
      ("c" . smart-comment)
      ("w" . save-buffer)
      ("a" . eglot-code-action-inline)
      ("o" . vemacs/dired)
      ("f" . vemacs/find-file)
      ("h" . replace-string)
      ("d" . consult-flymake)
      ("t" . eat)
      ("T" . eat-project)
      ("r" . async-shell-command)
      ("m" . command)
      ("p" . projectile-switch-project)
      ;; ("u" . undo-tree)
      ("s" . vemacs/auto-split-window)))

  (dolist (binding my-keybindings)
    (general-nmap
      :prefix "SPC"
      (car binding) (cdr binding))
    (general-vmap
      :prefix "SPC"
      (car binding) (cdr binding))))

;; Dired
;; (use-package dired
;; :ensure nil
;; :bind
;; (("M-RET" . dired-display-file)
;; ("h" . dired-up-directory)
;; ("H" . mono/close-dired)
;; ("l" . mono/dired-open)
;; ("L" . mono/dired-open-split)
;; ("m" . mono/dired-toggle-mark)
;; ("t" . dired-toggle-marks)
;; ("f" . dired-do-search)           ; Search marked files
;; ("a" . mono/dired-create-path)
;; ("y" . mono/dired-do-yank)
;; ("p" . mono/dired-do-paste)
;; ("c" . diredp-do-command-in-marked)
;; ("." . dired-mark-files-regexp)
;; ("s" . dired-mark-extension)
;; ("G" . diredp-do-grep-recursive)
;; ("Y" . dired-do-copy)
;; ("D" . dired-do-delete)
;; ("J" . dired-goto-file)
;; ("M" . dired-do-chmod)
;; ("O" . dired-do-chown)
;; ("P" . dired-do-print)
;; ("R" . dired-do-rename)
;; ("T" . dired-do-touch)
;; ("C" . dired-copy-filenamecopy-filename-as-kill) ; copies filename to kill ring.
;; ("Z" . dired-do-compress)
;; ("+" . dired-create-directory)
;; ("-" . dired-do-kill-lines)
;; ("% l" . dired-downcase)
;; ("% m" . dired-mark-files-regexp)
;; ("% u" . dired-upcase)
;; ("* %" . dired-mark-files-regexp)
;; ("* ." . dired-mark-extension)
;; ("* /" . dired-mark-directories)
;; ("; d" . epa-dired-do-decrypt)
;; ("; e" . epa-dired-do-encrypt)))
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map 
              ("M-RET" . dired-display-file)
              ("h" . dired-up-directory)
              ("H" . mono/close-dired)
              ("l" . mono/dired-open)
              ("L" . mono/dired-open-split)
              ("m" . mono/dired-toggle-mark)
              ("t" . dired-toggle-marks)
              ("f" . dired-do-search)
              ("a" . mono/dired-create-path)
              ("y" . mono/dired-do-yank)
              ("p" . mono/dired-do-paste)
              ("c" . diredp-do-command-in-marked)
              ("." . dired-mark-files-regexp)
              ("s" . dired-mark-extension)
              ("G" . diredp-do-grep-recursive)
              ("Y" . dired-do-copy)
              ("D" . dired-do-delete)
              ("J" . dired-goto-file)
              ("M" . dired-do-chmod)
              ("O" . dired-do-chown)
              ("P" . dired-do-print)
              ("R" . dired-do-rename)
              ("T" . dired-do-touch)
              ("C" . dired-copy-filename-as-kill)
              ("Z" . dired-do-compress)
              ("+" . dired-create-directory)
              ("-" . dired-do-kill-lines)
              ("% l" . dired-downcase)
              ("% m" . dired-mark-files-regexp)
              ("% u" . dired-upcase)
              ("* %" . dired-mark-files-regexp)
              ("* ." . dired-mark-extension)
              ("* /" . dired-mark-directories)
              ("; d" . epa-dired-do-decrypt)
              ("; e" . epa-dired-do-encrypt)))

;; PDF Tools
(use-package pdf-tools
  :config
  (pdf-tools-install))

(provide 'init)
;;; init.el ends here
