;; Note: describe-key commando gebruiken om te zien welke functie aan een
;; kb gebonden is

; (global-set-key (kbd "<ESC>") 'keyboard-quit)

;; evil surround
(define-key evil-visual-state-map "s" 'evil-surround-region)

;; Indent region
(define-key evil-visual-state-map "<tab>" 'indent-region)
(define-key evil-visual-state-map "<TAB>" 'indent-region)

;; Left shift
(define-key evil-visual-state-map "<" 'evil-shift-left)


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

(defun duckduckgo ()
  (interactive "")
  (eww "duckduckgo.com"))

(defun google ()
  (interactive "")
  (eww "google.com"))

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
  "fw" 'find-intero-whitelist-file
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

  ;; error navigation
  "el" 'flycheck-list-errors
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error

  ; Web
  "Wu" 'eww
  "Wd" 'duckduckgo
  "Wg" 'google
  "Wp" 'eww-back-url
  "Wn" 'eww-forward-url

  ; frames
  "Fn" 'make-frame-command
  "Fd" 'delete-frame
  "FF" 'other-frame

  ; region
  "ri" 'indent-region
  "ra" 'align-regexp

  ; xref (goto)
  "gd" 'xref-find-definitions
  "gD" 'xref-find-definitions-other-window
  "gr" 'xref-find-references
  )


;; Haskell keybindings
(evil-leader/set-key-for-mode 'haskell-mode
  "m0" 'haskell-navigate-imports
  "mt" 'dante-type-at
  "mi" 'dante-info
  "me" 'dante-eval-block)

;; LaTeX keybindings
(evil-leader/set-key-for-mode 'latex-mode
  "me" 'tex-latex-block)

;; eww keybindings
(evil-leader/set-key-for-mode 'eww-mode
  "mq" 'eww-quit
  "mr" 'eww-reload
  "my" 'eww-copy-page-url
  "mR" 'eww-readable
  "md" 'eww-download
  "mh" 'eww-back-url
  "ml" 'eww-forward-url
  )

;; dired keybindings
(evil-leader/set-key-for-mode 'dired-mode
  "<RET>" 'dired-find-alternate-file
  "<return>" 'dired-find-alternate-file)

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

;; Autocomplete keybindings\
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-SPC" ) 'company-complete-selection))
