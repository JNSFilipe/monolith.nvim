;;; ../../Documents/GitHub/monolith.nvim/demacs/anchor.el -*- lexical-binding: t; -*-
(defvar anchor/last-buffer nil "Buffer from which `anchor/goto-next` or `anchor/goto-previous` was last called.")
(defvar anchor/last-position nil "Position in `anchor/last-buffer` before the last call to `anchor/goto-next` or `anchor/goto-previous`.")


;; Drop anchor
(defun anchor/drop ()
  "Insert a new line with a commented anchor."
  (interactive)
  (end-of-line) ; Move to the end of the current line
  (newline-and-indent) ; Create a new line and indent
  (insert " <++>") ; Insert the anchor
  (comment-line 1) ; Comment the line)
  (indent-line)
  (setq anchor/last-buffer (current-buffer))
  (setq anchor/last-position (point))
  (backward-char 4)) ; Move the cursor before the anchor

;; Search anchor in buffer
(defun anchor/search-buffer ()
  "Search and list all occurrences of the anchor <++> in the current buffer."
  (interactive)
  (if (not (fboundp 'consult-line))
      (occur "<\\+\\+>")
    (consult-line "<\\+\\+>")
    (setq anchor/last-buffer (current-buffer))
    (setq anchor/last-position (point))))

;; Search anchor in project
(defun anchor/search-project ()
  "Search for the anchor <++> across all files in the current Projectile project."
  (interactive)
  (if (and (fboundp 'consult-ripgrep)
           (projectile-project-p)
           (setq anchor/last-buffer (current-buffer))
           (setq anchor/last-position (point)))
      (consult-ripgrep (projectile-project-root) "<\\+\\+>")
    (message "consult-ripgrep not available or not in a Projectile project")))

(defun anchor/last-jump ()
  "Toggle back to the last buffer and position visited by `anchor/goto-next` or `anchor/goto-previous`, and save the current position."
  (interactive)
  (let ((current-buffer (current-buffer))
        (current-position (point)))
    (if (and anchor/last-buffer
             (buffer-live-p anchor/last-buffer))
        (progn
          ;; Switch to the previous buffer and position
          (switch-to-buffer anchor/last-buffer)
          (goto-char anchor/last-position)
          ;; Update last-buffer and last-position to the current ones
          (setq anchor/last-buffer current-buffer)
          (setq anchor/last-position current-position))
      (message "No recorded last anchor buffer."))))

;; Go to next anchor in buffer
(defun anchor/next ()
  "Go to the next occurrence of the anchor <++> in the current buffer."
  (interactive)
  (search-forward "<++>" nil t))

;; Go to previous anchor in buffer
(defun anchor/prev ()
  "Go to the previous occurrence of the anchor <++> in the current buffer."
  (interactive)
  (search-backward "<++>" nil t))

