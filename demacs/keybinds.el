
;; Keybinds!
(defvar custom-shortcuts t)

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
      (map! :leader "g" nil)
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
;; (if custom-shortcuts)
