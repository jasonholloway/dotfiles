(require `package)
(setq package-enable-at-startup nil)
(add-to-list `package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list `package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
;;(add-to-list `package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
		(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up))

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


(use-package ensime
  :ensure t
  :pin melpa
  :config
    (setq ensime-startup-notification `nil))

;; (use-package scala-mode2 :ensure t)

(use-package projectile
  :ensure t
  :config
    (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :requires helm
  :requires projectile
  :config
    (set projectile-completion-system `helm)
    (helm-projectile-on))

(use-package dockerfile-mode
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(global-linum-mode t)
(setq linum-format "%d ")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(evil-shift-width 2)
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
	 (quote
		(projectile-ripgrep typescript-mode csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil)))
 '(scroll-error-top-bottom t)
 '(tab-width 2))



