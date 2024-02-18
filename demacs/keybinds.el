
;; Keybinds!
(defvar custom-shortcuts t)

(defun mono/find-file ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively 'projectile-find-file)
    (call-interactively 'find-file)))

(defun mono/dired ()
  (interactive)
  (if (projectile-project-p)
      (dired (projectile-project-root))
    (dired "~/")))

(defun mono/list-buffers ()
  (interactive)
  (if (projectile-project-p)
      (call-interactively '+vertico/switch-workspace-buffer) ; or '+helm/switch-workspace-buffer for Helm users
    (call-interactively 'ibuffer))) ; or another buffer listing command as per preference

(defun mono/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun mono/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun mono/xy-window-pixel-ratio ()
  "Return the ratio of the window's width to its height in pixels."
  (interactive)
  (let* ((edges (window-pixel-edges))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges)))
         (ratio (/ (float width) height)))
    (if (called-interactively-p 'interactive)
        (message "Width/Height Ratio: %f" ratio)
      ratio)))

;; (defun mono/auto-split-window ()
;;   (interactive)
;;   ;; Prompt for file selection
;;   (let ((file (read-file-name "Select a file: ")))
;;     ;; Determine split direction based on window dimensions
;;     (if (> (window-width) (window-height))
;;         (split-window-horizontally)  ; Wider window, split horizontally
;;       (split-window-vertically))    ; Taller window, split vertically
;;     ;; Open the selected file in the new window
;;     (other-window 1)
;;     (find-file file)))
(defun mono/auto-split-window ()
  (interactive)
  ;; Determine split direction based on window dimensions
  (if (> (mono/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  ;; Open a file in the new window using projectile-find-file
  (other-window 1)
  (projectile-find-file))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Bind Esc to abort minibuffers
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Bind Change project to C-Space
(global-set-key (kbd "C-SPC") #'projectile-switch-project)
(global-set-key (kbd "C-a") #'projectile-switch-open-project)

;; Bind keys for font size adjustment
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-set)

;; Bind C-q to kill the current buffer
(global-set-key (kbd "C-q") #'kill-this-buffer)

;; Jump through git hunks
(after! git-gutter
  (map! "º" #'git-gutter:next-hunk)
  (map! "ª" #'git-gutter:previous-hunk))

;; Keep selection after unindent
(after! evil
  ;; Map tab key to indent region when in visual mode
  (define-key evil-visual-state-map (kbd "<tab>") 'mono/evil-shift-right)
  (define-key evil-visual-state-map (kbd "<backtab>") 'mono/evil-shift-left)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  ;; Escape insert with jj and jk
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;; Disable default shortcuts
(when custom-shortcuts
  (map! :leader "RET" nil)
  (map! :leader "SPC" nil)
  (map! :leader "TAB" nil)
  (map! :leader "'" nil)
  (map! :leader "*" nil)
  (map! :leader "," nil)
  (map! :leader "." nil)
  (map! :leader "/" nil)
  (map! :leader ":" nil)
  (map! :leader ";" nil)
  (map! :leader "<" nil)
  (map! :leader "<" nil)
  (map! :leader "`" nil)
  (map! :leader "a" nil)
  (map! :leader "b" nil)
  (map! :leader "c" nil)
  (map! :leader "f" nil)
  ;; (map! :leader "g" nil)
  (map! :leader "h" nil)
  (map! :leader "i" nil)
  (map! :leader "n" nil)
  (map! :leader "o" nil)
  (map! :leader "p" nil)
  (map! :leader "r" nil)
  (map! :leader "s" nil)
  (map! :leader "t" nil)
  (map! :leader "u" nil)
  (map! :leader "w" nil)
  (map! :leader "x" nil)
  (map! :leader "X" nil)
  (map! :leader "~" nil)
  (map! :leader "q" nil))

;; New shortcuts
(when custom-shortcuts
  (map! :leader
        :desc "M-x" ":" #'execute-extended-command
        :desc "Eval" ";" #'eval-expression
        :desc "Scratch Buffer" "," #'doom/open-scratch-buffer
        :desc "All Buffers/Files" "." #'switch-to-buffer
        :desc "Search Buffer" "/" #'+default/search-buffer
        :desc "Search Project" "*" #'+default/search-project
        :desc "Code Actions" "a" #'lsp-execute-code-action
        :desc "Dired" "o" #'mono/dired
        :desc "Files" "f" #'mono/find-file
        :desc "Buffers" "b" #'mono/list-buffers
        :desc "EShell" "t" #'+eshell/toggle
        :desc "Toggle Comment" "c" #'comment-line
        :desc "Diagnostics" "d" #'consult-lsp-diagnostics
        :desc "Replace" "h" #'replace-string
        :desc "Run" "r" #'async-shell-command
        :desc "Make" "m" #'+make/run
        :desc "Split Window" "s" #'mono/auto-split-window
        :desc "Undo Tree" "u" #'undo-tree-visualize
        :desc "Yanks" :n "y" #'consult-yank-pop
        :desc "Yanks" :v "y" #'consult-yank-replace))
