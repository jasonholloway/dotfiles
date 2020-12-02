(load-file "init.d/packages.el")
(load-file "init.d/windows.el")
(load-file "init.d/keys.el")

;; various packages
(use-package monokai-theme
  :config (load-theme `monokai t))

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

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init (setq nameless-private-prefix t))

(use-package shut-up)

(use-package recentf
  :requires shut-up
  :config
  (recentf-mode 1)
  (run-at-time nil 5 (lambda () (shut-up 'recentf-save-list))))

(use-package beacon
  :config (beacon-mode t))

(use-package profile-dotemacs
  :quelpa
  ((profile-dotemacs
    :fetcher github
    :repo "raxod502/profile-dotemacs")))



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


(unless is-windows
  (use-package magit
    :config
    (general-define-key
     "C-x g" 'magit-status
     "C-x M-g" 'magit-dispatch)))


(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package terraform-mode)

(use-package yaml-mode)

(use-package salt-mode)

(use-package nginx-mode)

(use-package lsp-mode)


;; go
(use-package go-mode
  :requires lsp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred))

;; asp
(use-package pasp-mode
  :quelpa ((pasp-mode :fetcher github :repo "llaisdy/pasp-mode")))


(load-file "init.d/markdown.el")
(load-file "init.d/csharp.el")
(load-file "init.d/typescript.el")
(load-file "init.d/haskell.el")


(add-hook 'find-file-hook 'infer-indents)

(defun infer-indents ()
  "Chooses tabs or spaces depending on buffer."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (cond ((> space-count tab-count) (setq indent-tabs-mode nil))
          ((> tab-count space-count) (setq indent-tabs-mode t))
          (t (setq indent-tabs-mode nil)))))

;; common
(global-display-line-numbers-mode t)
;; (setq linum-format "%d ")               
(setq visible-bell 1)
(setq vc-follow-symlinks nil)

(winner-mode 1)
(windmove-default-keybindings `meta)

(setq backup-directory-alist `(("." . "~/.backups")))
(setq-default tab-width 2)
(setq-default standard-indent 2)

(menu-bar-mode 0)
(tool-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(setq org-hide-emphasis-markers t)

(if (display-graphic-p)
  (progn
    (scroll-bar-mode 0)
    (toggle-frame-fullscreen)))

(setq tramp-default-method "ssh")

(load-file "init.d/amigo.el")
(load-file "init.d/server.el")
(load-file "init.d/treemacs.el")

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
 '(org-hide-leading-stars t)
 '(org-image-actual-width 100)
 '(package-selected-packages
   (quote
    (helm-gtags ggtags amigo nameless names treemacs-projectile treemacs neo-tree beacon smooth-scrolling sublimity dante profile-dotemacs general evil-collection smartparens which-key quelpa-use-package quelpa impatient-mode esup shut-up lsp-mode go-mode projectile-ripgrep typescript-mode csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil)))
 '(scroll-error-top-bottom t)
 '(tab-width 2))

