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
(set-face-attribute 'default nil :family "Inconsolata" :height 130)

;; adwaita theme
(load-theme 'adwaita t)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; no tabs
(setq indent-tabs-mode nil)

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
    (helm use-package smooth-scrolling linum-relative intero evil-surround evil-magit evil-leader company-auctex))))
