;; Note: describe-key commando gebruiken om te zien welke functie aan een
;; kb gebonden is
;; General keybindings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ; buffers
  "bb" 'switch-to-buffer
  "bp" 'switch-to-prev-buffer
  "bn" 'switch-to-next-buffer
  "bk" 'kill-buffer

  ; windows
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "wd" 'delete-window
  "w|" 'split-window-horizontally
  "w-" 'split-window-vertically
  )

;; Flycheck keybindings
(evil-leader/set-key
  "el" 'flycheck-list-errors
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  )

;; Haskell keybindings
(evil-leader/set-key-for-mode 'haskell-mode
  "m0" 'haskell-navigate-imports
  "mg" 'intero-goto-definition
  "mt" 'intero-type-at
  "mi" 'intero-info
  "ml" 'intero-repl-load
  )

;; git keybindings
(evil-leader/set-key
  "gs" 'magit-status
  )

;; Outline keybindings
(evil-leader/set-key
  "ot" 'outline-toggle-children
  "oa" 'show-all
  "os" 'show-subtree
  "od" 'hide-subtree
  )
