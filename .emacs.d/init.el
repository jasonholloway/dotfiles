(require `package)
(add-to-list `package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list `package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list `package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package evil)

(load-theme `monokai t)  

(tool-bar-mode 0)
(menu-bar-mode 0)
(toggle-frame-fullscreen)
(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(global-set-key (kbd "M-]") `next-buffer)
(global-set-key (kbd "M-[") `previous-buffer)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(require `key-chord)
(key-chord-mode 1)
(key-chord-define-global "fm" `list-buffers)

(windmove-default-keybindings `meta)

(require `helm)
(global-set-key (kbd "M-x") `helm-M-x)
(global-set-key (kbd "C-x C-m") `helm-M-x)
(global-set-key (kbd "C-c C-m") `helm-M-x)

(require `recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" `recentf-open-files)
(run-at-time nil 120 `recentf-save-list)

(global-set-key (kbd "C-x b") `helm-mini)
(global-set-key (kbd "M-l") `helm-mini)
(key-chord-define-global "fm" `helm-mini)

(global-set-key (kbd "C-x C-f") `helm-find-files)

(setq helm-mini-default-sources `(helm-source-buffers-list
				  helm-source-recentf
				  helm-source-bookmarks
				  helm-source-buffer-not-found))

(use-package ensime :ensure t :pin melpa)
;; (use-package scala-mode2 :ensure t)

(use-package projectile)
(use-package helm-projectile :ensure t)

(require `helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system `helm)
(helm-projectile-on)

(require `evil)
(evil-mode 1)

(setq ensime-startup-notification `nil)

(global-linum-mode t)
(setq linum-format "%d ")

(require `js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook `js2-mode-hook `js2-imenu-extras-mode)

(require `js2-refactor)
(require `xref-js2) 
(add-hook `js2-mode-hook `js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") `js2r-kill)
(define-key js-mode-map (kbd "M-.") nil)
(add-hook `js2-mode-hook (lambda ()
				(add-hook `xref-backend-functions `xref-js2-xref-backend nil t)))
