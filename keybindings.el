;; Note: describe-key commando gebruiken om te zien welke functie aan een
;; kb gebonden is

; (global-set-key (kbd "<ESC>") 'keyboard-quit)

;; evil surround
(define-key evil-visual-state-map "s" 'evil-surround-region)

;; Indent region
; TODO this doesn't work
; (define-key evil-visual-state-map "<tab>" 'indent-region)
; (define-key evil-visual-state-map "<TAB>" 'indent-region)

;; Left shift
(define-key evil-visual-state-map "<" 'evil-shift-left)

(global-set-key (kbd "<escape>") 'keyboard-quit)

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
  "<SPC>" 'helm-M-x

  ; documentation
  "hk" 'describe-key
  "hp" 'info-display-manual
  "hf" 'describe-function
  "hc" 'describe-char
  "hv" 'describe-variable
  "hm" 'describe-mode

  ; files
  "fr" 'recentf-open-files
  "fR" 'rename-file-and-buffer 
  "fi" 'find-init-file
  "fk" 'find-keybindings-file
  "fw" 'find-intero-whitelist-file
  "fl" 'load-file
  "ff" 'helm-find-files
  
  ; buffers
  "bb" 'switch-to-buffer
  "bp" 'switch-to-prev-buffer
  "bn" 'switch-to-next-buffer
  "bd" 'kill-buffer

  ; projects (projectile)
  "p" 'projectile-command-map
  ;"pf" 'projectile-find-file
  ;"pF" 'projectile-find-file-other-frame
  ;"pd" 'projectile-find-dir
  ;"pD" 'projectile-find-dir-other-frame

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
  "ww" 'delete-other-windows
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

  ; Language Server Protocol (LSP) keybindings
  "gd" 'lsp-find-definition
  "gr" 'lsp-find-references
  "gt" 'lsp-find-type-definition

  ; Install packages
  "ir" 'refresh-packages
  "ii" 'package-install

  ; xref (goto)
  ; "gd" 'xref-find-definitions
  ; "gD" 'xref-find-definitions-other-window
  ; "gr" 'xref-find-references
  )

;; Emacs lisp keybindings
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "mel" 'eval-last-sexp
  "mer" 'eval-region
  "mee" 'eval-expression
  "meb" 'eval-buffer)

;; Haskell keybindings
(evil-leader/set-key-for-mode 'haskell-mode
  "g0" 'haskell-navigate-imports)
  ; "mt" 'dante-type-at
  ; "mi" 'dante-info
  ; "me" 'dante-eval-block)

;; Agda keybindings
(evil-leader/set-key-for-mode 'agda2-mode
  "ml" 'agda2-load
  "mc" 'agda2-make-case
  "ma" 'agda2-auto-maybe-all
  "mr" 'agda2-refine
  "m," 'agda2-goal-and-context
  "mn" 'agda2-next-goal
  "mp" 'agda2-previous-goal
  "mg" 'agda2-give
  )

;; LaTeX keybindings
(defun TeX-bold()
  (interactive)
  (TeX-font nil ?\C-b))
(defun TeX-typewriter()
  (interactive)
  (TeX-font nil ?\C-t))
(defun TeX-emphasis()
  (interactive)
  (TeX-font nil ?\C-e))
(defun TeX-smallcaps()
  (interactive)
  (TeX-font nil ?\C-c))
(defun TeX-font-delete()
  (interactive)
  (TeX-font nil ?\C-d))
(evil-leader/set-key-for-mode 'latex-mode
  "me" 'LaTeX-environment
  "mm" 'TeX-insert-macro
  "mfb" 'TeX-bold
  "mfe" 'TeX-emphasis
  "mfc" 'TeX-smallcaps
  "mft" 'TeX-typewriter
  "mfd" 'TeX-font-delete
  "mcp" 'TeX-comment-or-uncomment-paragraph
  )

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

;; Autocomplete keybindings
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-SPC") 'helm-company)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (define-key company-active-map (kbd "S-<return>") 'company-complete-common)
  ;(define-key company-active-map (kbd "RET") nil)
  ;(define-key company-active-map (kbd "C-SPC") 'company-complete-selection)
  (define-key company-active-map (kbd "C-j") 'evil-complete-next)
  (define-key company-active-map (kbd "C-k") 'evil-complete-previous))

;; Transparency control
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(evil-leader/set-key "Ft" 'toggle-transparency)
;; (global-set-key (kbd "C-c t") 'toggle-transparency)
