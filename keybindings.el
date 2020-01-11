;; Note: describe-key commando gebruiken om te zien welke functie aan een
;; kb gebonden is

; (global-set-key (kbd "<ESC>") 'keyboard-quit)

;; evil surround
(define-key evil-visual-state-map "s" 'evil-surround-region)

;; Interactive function definitions
(defun find-init-file ()
  (interactive "")
  (find-file user-init-file))

(defun find-keybindings-file ()
  (interactive "")
  (find-file (concat user-emacs-directory "keybindings.el")))

(defun find-intero-whitelist-file ()
  (interactive "")
  (find-file (concat user-emacs-directory "intero-whitelist.el")))

;; General keybindings
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ; M-x
  "<SPC>" 'execute-extended-command

  ; documentation
  "hk" 'describe-key
  "hp" 'info-display-manual
  "hf" 'describe-function

  ; files
  "fr" 'recentf-open-files
  "fi" 'find-init-file
  "fk" 'find-keybindings-file
  "fi" 'find-intero-whitelist-file
  "fl" 'load-file
  
  ; buffers
  "bb" 'switch-to-buffer
  "bp" 'switch-to-prev-buffer
  "bn" 'switch-to-next-buffer
  "bd" 'kill-buffer

  ; windows
  "wh" 'windmove-left
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wl" 'windmove-right
  "wn" 'next-multiframe-window
  "wp" 'previous-multiframe-window
  "wd" 'delete-window
  "w/" 'split-window-horizontally
  "w-" 'split-window-vertically
  "ww" 'other-window
  "w1" 'delete-other-windows

  ; frames
  "Fn" 'make-frame-command
  "Fd" 'delete-frame
  "FF" 'other-frame
  )

;; Flycheck keybindings
(evil-leader/set-key-for-mode 'flycheck-mode
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
