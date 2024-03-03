;;; ../../Documents/GitHub/monolith.nvim/demacs/wm.el -*- lexical-binding: t; -*-

(load-file "~/.config/doom/defs.el")

(when enable-exwm

  (defun mono/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun mono/exwm-init-hook ()
    ;; Set mouse speed
    (mono/set-mouse-speed)

    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1)

    ;; Open eshell by default
    (eshell)

    ;; Show battery status in the mode line
    (display-battery-mode 1)

    ;; Show the time and date in modeline
    (setq display-time-day-and-date t)
    (display-time-mode 1)

    ;; Launch apps that will run in the background
    (mono/run-in-background "nm-applet")
    (mono/run-in-background "blueman-applet"))

  (defun mono/launcher ()
    "Launch an application using Vertico."
    (interactive)
    ;; Get the list of installed applications (this might need refinement)
    (let ((app-list (split-string (shell-command-to-string "pacman -Qq") "\n" t)))
      (when-let ((app (completing-read "Launch application: " app-list)))
        (start-process "" nil app))))

  (defun mono/kill-buffer-and-window ()
    (interactive)
    (kill-buffer-and-window))

  (defun mono/delete-other-windows ()
    (interactive)
    (delete-other-windows))

  (defun mono/set-mouse-speed ()
    "Set mouse speed for EXWM."
    (interactive)
    ;; Replace "Your Mouse Device Name" and the property and value as needed
    (start-process-shell-command "xinput" nil "xinput set-prop 'ELAN1201:00 04F3:3098 Touchpad' 'libinput Accel Speed' 0.5"))


  (require 'exwm)
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)
  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'mono/exwm-init-hook)
  (require 'exwm-config)
  ;; (exwm-config-default)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "eDP"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output eDP --primary --mode 1920x1080 -r 60 --pos 0x0 --rotate normal --output HDMI1 --off --output VIRTUAL1 --off")))
  (exwm-randr-enable)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (map! "M-d" #'mono/launcher
        "M-q" #'mono/kill-buffer-and-window
        "M-f" #'mono/delete-other-windows)
  (exwm-input-set-key (kbd "M-d") 'mono/launcher)
  (exwm-input-set-key (kbd "M-q") 'mono/kill-buffer-and-window)
  (exwm-input-set-key (kbd "M-f") 'mono/delete-other-windows)
  (setq exwm-input-prefix-keys
        '(?\C-u
          ?\C-q
          ?\C-h
          ?\M-x
          ?\M-&
          ?\M-:
          ?\C-\ )) ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\M-\s] 'exwm-input-send-next-key)


  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `((, (kbd "C-h") . windmove-left)
          (, (kbd "C-l") . windmove-right)
          (, (kbd "C-k") . windmove-up)
          (, (kbd "C-j") . windmove-down)
          ;; Shortcuts
          (, (kbd "M-d") . mono/launcher)
          (, (kbd "M-q") . mono/kill-buffer-and-window)
          (, (kbd "M-f") . mono/delete-other-windows)

          ;; 'M-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))

          ;; 'M-s-N': Move window to, and switch to, a certain workspace.
          ,@(cl-mapcar (lambda (c n)
                         `(,(kbd (format "M-s-%c" c)) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-move-window ,n)
                             (exwm-workspace-switch ,n))))
                       '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                       ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                       (number-sequence 0 9))))


  ;; (setq exwm-input-global-keys
  ;;       `(
  ;;         ;; Move between windows
  ;;         ([C-h] . windmove-left)
  ;;         ([C-l] . windmove-right)
  ;;         ([C-k] . windmove-up)
  ;;         ([C-j] . windmove-down)

  ;;         ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
  ;;         ,@(mapcar (lambda (i)
  ;;                     `(,(kbd (format "s-%d" i)) .
  ;;                       (lambda ()
  ;;                         (interactive)
  ;;                         (exwm-workspace-switch-create ,i))))
  ;;                   (number-sequence 0 9))))
  (exwm-enable)

  (use-package desktop-environment
    :after exwm
    :config (desktop-environment-mode)))
