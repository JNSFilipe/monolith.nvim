
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

;; Disable default shortcuts
(if custom-shortcuts
    (after! which-key
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
      (map! :leader "q" nil)))

;; New shortcuts
(if custom-shortcuts
    (after! which-key
      (map! :map global-map "C-q" #'kill-this-buffer)
      (map! :leader ":" #'execute-extended-command)
      (map! :leader ";" #'eval-expression)
      (map! :leader "o" #'mono/dired)
      (map! :leader "f" #'mono/find-file)
      (map! :leader "b" #'mono/list-buffers)
      (map! :leader "t" #'+eshell/toggle)
      (map! :leader "c" #'comment-line)
      (map! :leader "r" #'async-shell-command)))
