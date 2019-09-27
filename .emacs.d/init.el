(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-hook 'find-file-hook 'infer-indents)
(defun infer-indents ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


(use-package magit
  :ensure t
  :config
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch))

;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (setq package-load-list '(all))
;;(unless (package-installed-p 'org)
;;  (package-install 'org))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copyingwhen-linked t)

(setq tramp-default-method "ssh")

(setq-default tab-width 2)
(setq-default standard-indent 2)

(use-package evil
  :ensure t
  :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (evil-mode 1)
    (setq-default evil-shift-width 2))

(use-package monokai-theme
  :ensure t
  :config
    (load-theme `monokai t))

(use-package flycheck
	:ensure t
	:config
	  (global-flycheck-mode))
	  ;; (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package company
  :ensure t
	:config
		(add-hook 'after-init-hook 'global-company-mode)
		(global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
		(setq company-idle-delay 0))

(use-package csharp-mode
  :ensure t
	:config
	  (add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode)))

(use-package omnisharp
  :ensure t
  :after (flycheck company csharp-mode)
  :config
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-hook 'csharp-mode-hook 'company-mode)
    (add-hook 'csharp-mode-hook 'flycheck-mode)
    (add-to-list 'company-backends 'company-omnisharp)
    (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
    (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))

(use-package org-re-reveal
	:ensure t
	:after org
	:config
		(add-hook 'after-save-hook
			(lambda ()
				(if reveal-mode
						(progn
							(org-re-reveal-export-to-html)
							(message "re-rendered!"))))))

(use-package htmlize
  :ensure t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-fullscreen)
(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(global-set-key (kbd "M-]") `next-buffer)
(global-set-key (kbd "M-[") `previous-buffer)

(windmove-default-keybindings `meta)

(use-package key-chord
	:ensure t
	:config
		(key-chord-mode 1)
		(key-chord-define-global "fm" `list-buffers))

(use-package helm
	:ensure t
	:config
		(global-set-key (kbd "M-x") `helm-M-x)
		(global-set-key (kbd "C-x C-m") `helm-M-x)
		(global-set-key (kbd "C-c C-m") `helm-M-x)
		(global-set-key (kbd "C-x b") `helm-mini)
		(global-set-key (kbd "M-l") `helm-mini)
		(key-chord-define-global "fm" `helm-mini)
		(global-set-key (kbd "C-x C-f") `helm-find-files)
		(setq helm-mini-default-sources `(helm-source-buffers-list
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

(use-package projectile
	:ensure t
	:config
		(projectile-mode))

(use-package helm-projectile
	:ensure t
	:after (helm projectile)
	:config
		(setq projectile-completion-system `helm)
		(helm-projectile-on))

(use-package terraform-mode
	:ensure t)

(use-package salt-mode
	:ensure t)

(use-package nginx-mode
	:ensure t)

(use-package markdown-mode
	:ensure t)

(use-package neotree
  :ensure t
  :config
    (global-set-key [f8] `neotree-toggle))

(global-linum-mode t)
(setq linum-format "%d ")


;; haskell stuff
(use-package haskell-mode
	:ensure t
	:config
		(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(use-package hindent
	:ensure t
	:after haskell-mode
	:config
	  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package intero
	:ensure t
	:after haskell-mode
	:config
	  (add-hook 'haskell-mode-hook 'intero-mode))


;; (use-package ghc
;; 	:after haskell-mode
;; 	:ensure t
;; 	:config
;; 		(add-hook 'haskell-mode-hook (lambda () (ghc-init))))

;; (use-package hare
;; 	:load-path "~/.cabal/share/x86_64-linux-ghc-8.0.2/HaRe-0.8.4.1/elisp"
;; 	:after ghc
;; 	:config
;; 	  (add-hook 'haskell-mode-hook (lambda () (hare-init))))


;; tramp gubbins
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
	(format "%s\\|%s"
		vc-ignore-dir-regexp
		tramp-file-name-regexp))
(setq tramp-verbose 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-image-actual-width 100)
 '(package-selected-packages
	 (quote
		(rainbow-delimiters powershell org-present dockerfile-mode markdown-mode nginx-mode salt-mode xref-js2 use-package terraform-mode omnisharp neotree monokai-theme magit key-chord js2-refactor htmlize helm-projectile evil ensime))))
