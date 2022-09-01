(require 'package)
(package-initialize)

(defun refresh-packages ()
  (interactive)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing defaults.
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(column-number-mode t)
(setq-default message-log-max nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; Word wrapping
(global-visual-line-mode t)

;; Show matching paren
(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil
                    :background nil
                    :foreground "#f00"
                    :underline nil
                    :bold :weight)
;(set-face-background 'show-paren-match nil)
;(set-face-underline 'show-paren-match "#000")
;(set-face-bold 'show-paren-match :weight)
(setq show-paren-delay 0)

;; Auto paired parens
(electric-pair-mode)

;; Less jumpy mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; font
(set-face-attribute 'default nil :family "Fira Code" :height 120)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; No tabs
(setq indent-tabs-mode nil)

;; Maximize frame
; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Advice to skip buffers
(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string= "*Messages*" (buffer-name))
    (next-buffer)))

;; When selecting a file in Dired, re-use the Dired buffer to display the file.
(put 'dired-find-alternate-file 'disabled nil)

;; .pl is prolog, not perl
(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON DEPENDENCIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(require 'popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vim keybindings.
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq which-key-allow-evil-operators 1)
(require 'evil)
(require 'evil-leader)
(require 'evil-surround)
(require 'evil-collection)
(global-evil-leader-mode 1)
(global-evil-surround-mode 1)
(evil-mode 1)
(evil-collection-init)
(load-file "~/.emacs.d/keybindings.el")

;; More sane undo system.
(require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode 1)

;; Show info on partially entered keybinding.
; TODO this doesn't work properly: currently no support for partially
; entered evil commands, and I don't know how to support the leader
; key properly (perhaps take a look at the "interactive" function?)
(require 'which-key)
(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE TWEAKING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling.
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Spacemacs theme.
(require 'spacemacs-dark-theme)

;; Relative line numbers.
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-mode)
(linum-relative-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOCOMPLETION AND OTHER AUTOMATION TOOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm.
(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; Company autocompletion.
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile (file search etc).
(require 'projectile)
(projectile-mode +1)

;; Git integration.
(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE SUPPORT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language server protocol.
(require 'lsp-mode)

;; Latex support.
(load "auctex.el" nil t t)
(load "preview.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'plain-TeX-mode-hook
          (lambda () (set (make-local-variable 'TeX-electric-math)
                          (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-local-variable 'TeX-electric-math)
                          (cons "$" "$"))))
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(setq TeX-electric-escape t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm: installed but should look at features more
;; company
;; magit
;; haskell
;; prolog
;; javascript/typescript
;; vue/angular
;; snippets

;; IDE: lsp-mode, company, flycheck, projectile

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   (quote
    (magit evil-collection company-auctex which-key auctex undo-tree spacemacs-theme smooth-scrolling linum-relative helm evil-surround evil-leader evil projectile company lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
