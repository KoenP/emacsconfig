;; info-display-manual
;;   -> Om een handleiding te verkennen


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
(package-refresh-contents)

(package-install 'undo-tree)
(package-install 'company)
(package-install 'linum-relative)
(package-install 'smooth-scrolling)
(package-install 'evil-leader)
(package-install 'evil-surround)
(package-install 'evil)
(package-install 'goto-chg)
(package-install 'intero)
(package-install 'auctex)
(package-install 'company-auctex)
(package-install 'magit)
(package-install 'evil-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable tool bar
(tool-bar-mode -1)

;; No cursor blink
(blink-cursor-mode 0)

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

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; Inconsolata font
(set-face-attribute 'default nil :family "Inconsolata" :height 140)

;; adwaita theme
(load-theme 'adwaita t)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree
(global-undo-tree-mode)

;; Vim key bindings
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode)
(global-evil-surround-mode 1)
(evil-mode 1)

;; Smooth scrolling
(smooth-scrolling-mode 1)

;; Relative line numbers
(setq linum-relative-current-symbol "")
(linum-mode)
(linum-relative-global-mode)

;; Haskell mode
(setq exec-path (append exec-path '("~/.local/bin")))
(custom-set-variables '(haskell-stylish-on-save t))
(load-file "~/.emacs.d/intero-whitelist.el")
(add-hook 'haskell-mode-hook 'intero-mode-whitelist)

;; Autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Latex
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)
(company-auctex-init)

;; git integration
; optional: this is the evil state that evil-magit will use
; (setq evil-magit-state 'normal)
; optional: disable additional bindings for yanking text
; (setq evil-magit-use-y-for-yank nil)
(require 'evil-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/keybindings.el")
