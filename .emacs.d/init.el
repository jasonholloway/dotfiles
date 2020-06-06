(require `package)

(add-to-list `package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list `package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list `package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default tab-width 2)
(setq-default standard-indent 2)

(add-hook 'find-file-hook 'infer-indents)
(defun infer-indents ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package magit
  :ensure t
  :config
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch))

(use-package company
  :ensure t
  :config
    (add-hook 'after-init-hook 'global-company-mode)
    (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
    (setq company-dabbrev-downcase nil)
    (setq company-idle-delay 0))

(use-package key-chord
  :ensure t
  :config
    (key-chord-mode 1)
    (key-chord-define-global "fm" `list-buffers))

(use-package evil
  :ensure t
  :requires key-chord
  :config
    (evil-mode 1)
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(use-package monokai-theme
  :ensure t
  :config (load-theme `monokai t))  

(use-package terraform-mode
	:ensure t)

(setq org-hide-emphasis-markers t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-fullscreen)
(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(global-set-key (kbd "M-]") `next-buffer)
;(global-set-key (kbd "M-[") `previous-buffer)

(windmove-default-keybindings `meta)

(use-package helm
  :ensure t
	:requires key-chord
  :config
    (global-set-key (kbd "M-x") `helm-M-x)
    (global-set-key (kbd "C-x C-m") `helm-M-x)
    (global-set-key (kbd "C-c C-m") `helm-M-x)
    (global-set-key (kbd "C-x b") `helm-mini)
    (global-set-key (kbd "M-l") `helm-mini)
    (key-chord-define-global "fm" `helm-mini)
    (global-set-key (kbd "C-x C-f") `helm-find-files)
    (setq helm-mini-default-sources
	  `(helm-source-buffers-list
	    helm-source-recentf
	    helm-source-bookmarks
	    helm-source-buffer-not-found)))

(use-package recentf
  :ensure t
  :config
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key "\C-x\ \C-r" `recentf-open-files)
    (run-at-time nil 120 `recentf-save-list))

(use-package projectile
  :ensure t
  :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (setq projectile-indexing-method 'native)
    (setq projectile-enable-caching t))

(use-package helm-ag
  :ensure t
  :config
    (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
    (setq helm-ag-command-option "--all-text")
    (setq helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :ensure t
  :requires helm
  :requires projectile
  :config
    (setq projectile-completion-system `helm)
    (helm-projectile-on))

(use-package dockerfile-mode
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package yaml-mode
  :ensure t)

(use-package lsp-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :requires lsp-mode
  :config
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
    (add-hook 'go-mode-hook 'lsp-deferred))

(global-linum-mode t)
(setq linum-format "%d ")

(require 'server)
(and (>= emacs-major-version 23)
	(defun server-ensure-safe-dir (dir) "Noop" t))

(setq visible-bell 1)

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
 '(package-selected-packages
	 (quote
		(projectile-ripgrep typescript-mode csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil)))
 '(scroll-error-top-bottom t)
 '(tab-width 2))



