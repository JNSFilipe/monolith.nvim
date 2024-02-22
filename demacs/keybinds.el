
;; Keybinds!
(defvar custom-shortcuts t)

(load-file "~/.config/doom/anchor.el")

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

(defun mono/auto-split-window ()
  (interactive)
  ;; Determine split direction based on window dimensions
  (if (> (mono/xy-window-pixel-ratio) 1.0)
      (split-window-horizontally)       ; Wider window, split horizontally
    (split-window-vertically))          ; Taller window, split vertically
  ;; Open a file in the new window using projectile-find-file
  (other-window 1)
  (projectile-find-file))

(defun mono/minibuffer-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun mono/navigate ()
  "Navigate to anchors in the project if they exist, otherwise use mono/find-file."
  (interactive)
  (if (and (projectile-project-p)
           (anchor/anchors-in-project))
      (call-interactively 'anchor/search-project)
    (call-interactively 'mono/find-file)))


(defun mono/tab-navigate ()
  "Navigate to anchors in the project if they exist, otherwise use mono/find-file."
  (interactive)
  (if (and (projectile-project-p)
           (not (anchor/no-last-buffer))
           (anchor/anchors-in-project))
      (call-interactively 'anchor/last-jump)
    (switch-to-buffer (other-buffer (current-buffer) 1))))

(defun mono/run-make-or-compile ()
  "Run `+make/run` if a Makefile exists, otherwise `compile`."
  (interactive)
  (if (file-exists-p (expand-file-name "Makefile" (doom-project-root)))
      (+make/run)
    (call-interactively 'compile)))

;; Make which-key appear faster
(setq which-key-idle-delay 0.5)

;; Bind Esc to abort minibuffers
(map! :map minibuffer-local-map "<escape>" #'mono/minibuffer-quit)
(map! :map minibuffer-local-ns-map "<escape>" #'mono/minibuffer-quit)
(map! :map minibuffer-local-completion-map "<escape>" #'mono/minibuffer-quit)
(map! :map minibuffer-local-must-match-map "<escape>" #'mono/minibuffer-quit)
(map! :map minibuffer-local-isearch-map "<escape>" #'mono/minibuffer-quit)

;; Config Copilot
(after! copilot
  (map! :i "C-l" #'copilot-accept-completion
        :i "C-k" #'copilot-accept-completion-by-word))

;; Keep selection after unindent
(after! evil
  ;; Map tab key to indent region when in visual mode
  (map! :v "<tab>" #'mono/evil-shift-right
        :v "<backtab>" #'mono/evil-shift-left
        :n "j" "gj" ;; Enable easier navigation in wrapped lines
        :n "k" "gk" ;; Enable easier navigation in wrapped lines
        :n "C-h" #'evil-window-left ;; TODO: This is not working in C files
        :n "C-l" #'evil-window-right
        :n "C-j" #'evil-window-down
        :n "C-k" #'evil-window-up)
  ;; Escape insert with jj and jk
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;; Jump through git hunks
(after! git-gutter
  (map! "º" #'git-gutter:next-hunk
        "ª" #'git-gutter:previous-hunk))

(map!
 ;; Manage project opening/switching
 "C-SPC" #'projectile-switch-project
 "C-a" #'projectile-switch-open-project
 ;; Kill current buffer
 "C-q" #'kill-this-buffer
 ;; Anchors
 "ç" #'anchor/next
 "Ç" #'anchor/prev
 :n "<tab>" #'mono/tab-navigate
 ;; Navigate buffers
 :n "<" #'previous-buffer
 :n ">" #'next-buffer)

;; Disable default shortcuts
(when custom-shortcuts
  (map! :leader
        "RET" nil
        "SPC" nil
        "TAB" nil
        "'" nil
        "*" nil
        "," nil
        "." nil
        "/" nil
        ":" nil
        ";" nil
        "<" nil
        "`" nil
        "a" nil
        "b" nil
        "c" nil
        "f" nil
        ;; "g" nil
        "h" nil
        "i" nil
        "n" nil
        "o" nil
        "p" nil
        "r" nil
        "s" nil
        "t" nil
        "u" nil
        "w" nil
        "x" nil
        "X" nil
        "~" nil
        "q" nil))

;; New shortcuts
(when custom-shortcuts
  (map! :leader
        :desc "M-x" ":" #'execute-extended-command
        :desc "Eval" ";" #'eval-expression
        :desc "Scratch Buffer" "," #'doom/open-scratch-buffer
        :desc "All Buffers/Files" "." #'switch-to-buffer
        :desc "Search Buffer" "/" #'+default/search-buffer
        :desc "Search Project" "*" #'+default/search-project
        :desc "Adjust Font" "'" #'text-scale-adjust
        :desc "Code Actions" "a" #'lsp-execute-code-action
        :desc "Dired" "o" #'mono/dired
        :desc "Files" "f" #'mono/find-file
        :desc "Buffers" "b" #'mono/list-buffers
        :desc "EShell" "t" #'+eshell/toggle
        :desc "Toggle Comment" "c" #'comment-line
        :desc "Diagnostics" "d" #'consult-lsp-diagnostics
        :desc "Replace" "h" #'replace-string
        :desc "Run" "r" #'async-shell-command
        :desc "Make" "m" #'mono/run-make-or-compile
        :desc "Split Window" "s" #'mono/auto-split-window
        :desc "Undo Tree" "u" #'undo-tree-visualize
        :desc "Yanks" :n "y" #'consult-yank-pop
        :desc "Yanks" :v "y" #'consult-yank-replace
        :desc "Anchors" "ç" nil
        :desc "Drop" "çç" #'anchor/drop
        :desc "Search in Buffer" "çp" #'anchor/search-buffer
        :desc "Navigate Files" "SPC" #'mono/navigate
        :desc "Notes" "n" nil
        :desc "Capture Note Daily" "nn" #'org-roam-dailies-capture-today
        :desc "Capture Note" "nc" #'org-roam-capture
        :desc "Org Agenda" "na" #'org-agenda
        :desc "Roam Find Note" "nr" #'org-roam-node-find
        :desc "Find in Agenda" "nf" #'consult-org-agenda
        :desc "New Roam Note" "ni" #'org-roam-node-insert
        :desc "Switch to Notes" "ns" (lambda () (interactive) (projectile-switch-project-by-name org-directory))))
