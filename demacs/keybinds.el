(load-file "~/.config/doom/defs.el")
(load-file "~/.config/doom/anchor.el")
(load-file "~/.config/doom/mono.el")

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

;; Config Dired
(after! dired
  (map! :map dired-mode-map
        :n "M-RET" #'dired-display-file
        :n "h" #'dired-up-directory
        :n "H" #'mono/close-dired
        :n "l" #'mono/dired-open
        :n "L" #'mono/dired-open-split
        :n "m" #'mono/dired-toggle-mark
        :n "t" #'dired-toggle-marks
        :n "f" #'dired-do-search           ; Search marked files
        :n "a" #'mono/dired-create-path
        :n "y" #'mono/dired-do-yank
        :n "p" #'mono/dired-do-paste
        :n "c" #'diredp-do-command-in-marked
        :n "." #'dired-mark-files-regexp
        :n "s" #'dired-mark-extension
        :n "G" #'diredp-do-grep-recursive
        :n "Y" #'dired-do-copy
        :n "D" #'dired-do-delete
        :n "J" #'dired-goto-file
        :n "M" #'dired-do-chmod
        :n "O" #'dired-do-chown
        :n "P" #'dired-do-print
        :n "R" #'dired-do-rename
        :n "T" #'dired-do-touch
        :n "C" #'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
        :n "Z" #'dired-do-compress
        :n "+" #'dired-create-directory
        :n "-" #'dired-do-kill-lines
        :n "% l" #'dired-downcase
        :n "% m" #'dired-mark-files-regexp
        :n "% u" #'dired-upcase
        :n "* %" #'dired-mark-files-regexp
        :n "* ." #'dired-mark-extension
        :n "* /" #'dired-mark-directories
        :n "; d" #'epa-dired-do-decrypt
        :n "; e" #'epa-dired-do-encrypt))

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
        :desc "Capture Note Daily" "nn" #'obsidian-daily-note
        :desc "Capture Note" "nc" #'obsidian-capture
        :desc "Find in Notes" "nf" #'obsidian-jump
        ;; TODO: make shortcut to find TODOs in notes
        :desc "Switch to Notes" "ns" (lambda () (interactive) (projectile-switch-project-by-name obsidian-notes-dir))))
