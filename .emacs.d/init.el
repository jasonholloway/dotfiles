(require `package)
(setq package-check-signature nil)
(package-initialize)

(add-to-list `package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list `package-archives '("marmalade" . "http://marmalade.ferrier.me.uk/packages/"))
(add-to-list `package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

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

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

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

(use-package yaml-mode)

(use-package terraform-mode)

(use-package lsp-mode)

(use-package go-mode
  :requires lsp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred))


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

(use-package impatient-mode
  :quelpa
  :requires simple-httpd
  :hook markdown-mode
  :config
    (httpd-start))


(use-package typescript-mode
  :config
  (setq typescript-indent-level 4)
  (define-key typescript-mode-map (kbd "C-c t r") 'tide-rename-symbol)
  (define-key typescript-mode-map (kbd "C-c t R") 'tide-rename-file))

(defun setup-tide ()
  "Setup Tide."
  (tide-setup)
  (flycheck-mode 1)
  (company-mode 1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . setup-tide))

(use-package web-mode
  :after flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  :hook
  (web-mode . (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide)))))


(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)


(setq org-hide-emphasis-markers t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(toggle-frame-fullscreen)
(fset `yes-or-no-p `y-or-n-p)

(windmove-default-keybindings `meta)

(global-linum-mode t)
(setq linum-format "%d ")
(setq visible-bell 1)

(winner-mode 1)

(setq backup-directory-alist `(("." . "~/.backups")))

(setq-default tab-width 2)
(setq-default standard-indent 2)

(add-hook 'find-file-hook 'infer-indents)
(defun infer-indents ()
  "Chooses tabs or spaces depending on buffer."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))


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
 '(package-selected-packages
   (quote
    (general evil-collection smartparens which-key quelpa-use-package quelpa impatient-mode esup shut-up lsp-mode go-mode projectile-ripgrep typescript-mode csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil)))
 '(scroll-error-top-bottom t)
 '(tab-width 2))

