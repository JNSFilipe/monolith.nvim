;;; ../../Documents/GitHub/monolith.nvim/demacs/defs.el -*- lexical-binding: t; -*-

(defun mono/exwm-enable-if-possible (enable)
  "Enable EXWM if ENABLE is t, running on GNU/Linux, and not under another desktop session."
  (if (and (eq enable t)
           (eq system-type 'gnu/linux)
           (not (getenv "DESKTOP_SESSION")))
      t
    nil))

;; Keybinds! Enable custom keybinds and disable Doom's default ones
(defvar custom-shortcuts t)

;; Enable EXWM stuff
(defvar enable-exwm (mono/exwm-enable-if-possible t))

;; Variable where path to Obsidian Notes folder is stored
(setq obsidian-notes-dir "~/Documents/GitHub/Ecthelion")
