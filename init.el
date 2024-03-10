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
(menu-bar-mode -1)
(blink-cursor-mode 0)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(electric-indent-mode t)
(setq-default c-basic-offset 2)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

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

;; Treat CamelCase words as multiple words.
;(global-subword-mode t)

;; Insert current date command.
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

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
(setq evil-want-fine-undo t)
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

;; Theme.
; (require 'spacemacs-dark-theme)
; (load-theme 'spacemacs-dark t nil)
(load-theme 'leuven t)

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
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)

;; Helm interface for company-mode.
(require 'helm-company)

;; Snippets.
(require 'yasnippet)

;; Inline error messages.
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq flycheck-display-errors-delay 0.0)
(setq flycheck-idle-change-delay 0.0)
(setq flycheck-idle-buffer-switch-delay 0.0)
(global-flycheck-mode)

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
;; Incremental parsing support.
(require 'tree-sitter)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'tree-sitter-langs)

;; Org mode config.
(setq org-image-actual-width '(400))
  ; Look for "#+attr_html: :width ...px" annotation, otherwise use default of 400px.
  ; See https://orgmode.org/manual/Images.html
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))
(setq org-startup-with-inline-images nil)
(setq org-return-follows-link t)

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
(setq TeX-electric-escape nil)


;; Vue 3 support. Requires volar language server.
;; npm install -g @volar/server
(require 'vue-mode)
;(add-hook 'vue-mode-hook #'lsp)


;; Haskell support. Requires haskell language server.
(require 'haskell-mode)
;(require 'lsp-haskell)
;(require 'flycheck-haskell)
(setq exec-path (append exec-path '("~/.local/bin")))
(setq haskell-indent-offset 2)
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'highlight-uses-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate--mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

;; C/C++ support.
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; Web development support (javascript/typescript/html/css).
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-auto-close-style 2)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(require 'typescript-mode)
(add-hook 'typescript-mode-hook 'lsp)
(setq typescript-indent-level 2)

;; Language server protocol.
(require 'lsp-mode)
(require 'lsp-ui)
(setq lsp-idle-delay 0.1)
(setq lsp-lens-enable nil) ; turn off massive import list suggestions, mostly for Haskell
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode)
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'bottom))
  ;(setq lsp-ui-doc-delay 0.1)
  ;(setq lsp-ui-doc-position 'at-point))
;; TODO
;; lsp ui peek feature?
;; lsp ui imenu feature?


;; Alternative client for language server protocol.
; (require 'eglot)

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
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(elm-indent-offset 2)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(elm-mode web-mode projectile-ripgrep ripgrep eglot helm-company vue-mode lsp-ui flycheck-haskell flycheck lsp-haskell haskell-mode tree-sitter-langs tree-sitter typescript-mode magit evil-collection company-auctex which-key auctex undo-tree spacemacs-theme smooth-scrolling linum-relative helm evil-surround evil-leader evil projectile company lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
