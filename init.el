;; info-display-manual
;;   -> Om een handleiding te verkennen

;; Note: describe-key commando gebruiken om te zien welke functie aan een
;; kb gebonden is

(setq evil-want-C-u-scroll t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD PATHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(let ((default-directory  "~/.emacs.d/lisp/"))
;  (normal-top-level-add-subdirs-to-load-path))


(require 'package)
 (add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-ensure-all t)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package undo-tree
   :config
   (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package company)

(use-package linum-relative
  :config
  (setq linum-relative-current-symbol "")
  (linum-mode)
  (linum-relative-global-mode))

(use-package smooth-scrolling)
(use-package evil-leader)
(use-package evil-surround)
(use-package evil
  :config
  (global-evil-leader-mode)
  (global-evil-surround-mode 1)
  (evil-mode 1))
(use-package goto-chg)
(use-package haskell-mode
  :config
  (setq exec-path (append exec-path '("~/.local/bin"))))

(use-package intero)

; (use-package auctex)
; (use-package company-auctex)
(use-package magit)
(use-package evil-magit
  :init
  ; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal)
  ; optional: disable additional bindings for yanking text
  (setq evil-magit-use-y-for-yank nil))

; (use-package dante
;   :after haskell-mode
;   :commands 'dante-mode
;   :init
;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;   (add-hook 'haskell-mode-hook 'dante-mode)
;   :config
;   (setq flymake-no-changes-timeout nil)
;   (setq flymake-start-syntax-check-on-newline nil)
;   (setq flycheck-check-syntax-automatically '(save mode-enabled)))


;(use-package lsp-haskell
;  :init
;  (require 'lsp)
;  :config
;  (add-hook 'haskell-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turning off some annoying defaults
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(column-number-mode t)

;; Word wrapping
(global-visual-line-mode t)

;; Show matching paren
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil
		    :background nil
		    :foreground "#f00"
		    :underline nil
		    :bold :weight
		    )
;(set-face-background 'show-paren-match nil)
;(set-face-underline 'show-paren-match "#000")
;(set-face-bold 'show-paren-match :weight)
(setq show-paren-delay 0)

;; Auto paired parens
(electric-pair-mode)

;; Less jumpy scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(smooth-scrolling-mode 1)

;; Inconsolata font
(set-face-attribute 'default nil :family "Fira Code" :height 120)

;; adwaita theme
(load-theme 'adwaita t)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; No tabs
(setq indent-tabs-mode nil)

;; Maximize frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Autofill
(setq current-fill-column 80)
(auto-fill-mode t)

;; Advice to skip buffers
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string= "*Messages*" (buffer-name))
    (next-buffer)))

;; When selecting a file in Dired, re-use the Dired buffer to display the file.
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C++ mode
(setq c-basic-offset 2)
(setq-default c-electric-flag nil)

;; Autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Latex
;(load "auctex.el" nil t t)
; (setq TeX-auto-save t)
; (setq TeX-parse-self t)
; (setq TeX-save-query nil)
; (setq TeX-PDF-mode t)
; (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
; (company-auctex-init)

;; git integration
(require 'evil-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/keybindings.el")



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
;  '(haskell-stylish-on-save t)
;  '(package-selected-packages
;    (quote
;     (magit auctex goto-chg evil company undo-tree smooth-scrolling linum-relative intero evil-surround evil-magit evil-leader company-auctex))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key dante lsp-haskell helm use-package smooth-scrolling linum-relative intero evil-surround evil-magit evil-leader company-auctex))))

;; Font Ligatures
(defun my-correct-symbol-bounds (pretty-alist)
    "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
    (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
            pretty-alist))

(defun my-ligature-list (ligatures codepoint-start)
    "Create an alist of strings to replace with
codepoints starting from codepoint-start."
    (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))

(setq my-fira-code-ligatures
    (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                "x" ":" "+" "+" "*")))
    (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

(defun my-set-fira-code-ligatures ()
    "Add fira code ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
        (append my-fira-code-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))

(add-hook 'prog-mode-hook 'my-set-fira-code-ligatures)
