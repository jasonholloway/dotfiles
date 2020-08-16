(require `package)
(setq package-check-signature nil)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(eval-when-compile
  (require 'quelpa-use-package)
  (require 'use-package))

(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)

(use-package monokai-theme
  :config (load-theme `monokai t))

(use-package general)

(use-package which-key
  :config
  (which-key-mode 1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package flycheck
  :init
  (setq flycheck-keymap-prefix (kbd "C-c e"))
  :config
  (global-flycheck-mode))

(use-package company
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete-common-or-cycle))

(use-package key-chord
  :config
  (key-chord-mode 1))


(use-package evil
  :requires key-chord
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))


(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(use-package shut-up)

(use-package recentf
  :requires shut-up
  :config
  (recentf-mode 1)
  (run-at-time nil 5 (lambda () (shut-up 'recentf-save-list))))


(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-use-git-grep t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t))


(use-package helm
  :config
  (global-set-key (kbd "M-x") `helm-M-x)
  (global-set-key (kbd "C-x b") `helm-mini)
  (global-set-key (kbd "C-x C-f") `helm-find-files)
  (setq helm-mini-default-sources
        `(helm-source-buffers-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-buffer-not-found)))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (setq projectile-completion-system `helm)
  (helm-projectile-on))

(use-package helm-ag
  :after helm
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq helm-ag-command-option "--all-text")
  (setq helm-ag-insert-at-point 'symbol))


(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package terraform-mode)

(use-package yaml-mode)

(use-package salt-mode)

(use-package nginx-mode)

(use-package lsp-mode)


;; markdown
(defun md2html (buffer)
  "Convert BUFFER of Markdown into a html string."
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(use-package simple-httpd
  :quelpa)

(use-package markdown-mode
  :requires impatient-mode
  :config (imp-set-user-filter 'md2html))

(setq exec-path (append exec-path '("/home/jason/.nvm/versions/node/v10.14.1/bin")))

(use-package impatient-mode
  :quelpa
  :requires simple-httpd
  :hook markdown-mode
  :config
    (httpd-start))


;; go
(use-package go-mode
  :requires lsp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred))


;; csharp
(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . csharp-mode))
  (add-to-list 'auto-mode-alist '("\\.sln\\'" . csharp-mode))
  (add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode)))

(use-package omnisharp
  :after (flycheck company csharp-mode)
  :config
  (setq omnisharp-server-executable-path "/opt/omnisharp/OmniSharp.exe")
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'company-mode)
  (add-hook 'csharp-mode-hook 'flycheck-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))


;; typescript stuff
(defun setup-tide ()
  "Setup Tide for any mode that does typescripty things."
  (tide-setup)
  (flycheck-mode 1)
  (company-mode 1)
  (tide-hl-identifier-mode +1)
  (general-define-key
   :states 'normal
   :keymaps 'local
   :prefix "C-c t"
   "f" 'tide-fix
   "F" 'tide-refactor
   "r" 'tide-rename-symbol
   "R" 'tide-rename-file
   "e" 'tide-error-at-point
   "E" 'tide-project-errors
   "g" 'tide-jump-to-definition
   "g" 'tide-jump-to-implementation
   "p" 'tide-format)
  (general-define-key
   :states 'normal
   :keymaps 'local
   "M-n" 'flycheck-next-error
   "M-p" 'flycheck-previous-error
   "M-l" 'flycheck-list-errors
   ))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 4))

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide)
         (web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide))))))

(use-package web-mode
  :after flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White"))


;; prolog stuff
(setq prolog-system 'swi
      prolog-program-switches '((swi ("-G128M" "-T128M" "-L128M" "-O"))
                                (t nil))
      prolog-electric-underscore-flag t)

(use-package ediprolog
  :config
  (setq ediprolog-system "swi")
  (general-define-key
   :states 'normal
   :keymaps 'prolog-mode
   :prefix "C-c"
   "e" 'ediprolog-dwim))

(use-package pasp-mode
  :quelpa ((pasp-mode :fetcher github :repo "https://github.com/llaisdy/pasp-mode") :update t))

;; haskell stuff
(use-package haskell-mode
  :config
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(use-package hindent
  :after haskell-mode
  :config
    (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package intero
  :after haskell-mode
  :config
    (add-hook 'haskell-mode-hook 'intero-mode))

(setq tramp-default-method "ssh")

(use-package helm-hoogle
  :after haskell-mode)


(add-hook 'find-file-hook 'infer-indents)
(defun infer-indents ()
  "Chooses tabs or spaces depending on buffer."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(cond
 ((string-equal system-type "windows-nt")
  (progn
    (setq gc-cons-threshold 100000000)
    (add-hook 'window-setup-hook 'toggle-frame-fullscreen t))))


(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)


;; common
(global-linum-mode t)
(setq linum-format "%d ")
(setq visible-bell 1)

(winner-mode 1)
(windmove-default-keybindings `meta)

(setq backup-directory-alist `(("." . "~/.backups")))
(setq-default tab-width 2)
(setq-default standard-indent 2)

(menu-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(setq org-hide-emphasis-markers t)

(if (display-graphic-p)
  (progn
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (toggle-frame-fullscreen)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sp-show-pair-match-face ((t (:foreground "#87D700" :inverse-video nil :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(evil-shift-width 2)
 '(org-hide-emphasis-markers t t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-image-actual-width 100)
 '(package-selected-packages
   (quote
    (general evil-collection smartparens which-key quelpa-use-package quelpa impatient-mode esup shut-up lsp-mode go-mode projectile-ripgrep typescript-mode csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil)))
 '(scroll-error-top-bottom t)
 '(tab-width 2))

