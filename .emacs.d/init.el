(load-file "~/.emacs.d/init.d/packages.el")
(load-file "~/.emacs.d/init.d/windows.el")
(load-file "~/.emacs.d/init.d/keys.el")

(add-to-list 'auto-mode-alist '("@.+\\'" . shell-script-mode))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

(add-to-list 'term-file-aliases '("foot" . "xterm"))

(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))


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
  (add-hook 'typescript-mode-hook 'flycheck-mode))

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

(use-package default-text-scale
  :config
  (default-text-scale-mode))

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
  (setq helm-minibuffer-history-key "M-p")
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
  (setq helm-ag-insert-at-point 'symbol)
  (setq helm-ag-use-agignore t)
  (setq helm-ag-ignore-patterns '())
  )


(unless is-windows
  (use-package magit
    :config
    (general-define-key
     "C-x g" 'magit-status
     "C-x M-g" 'magit-dispatch)))

(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-parens-as-block t)
;; (setq cperl-close-paren-offset (- cperl-indent-level))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package terraform-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(use-package cue-mode
  :config(add-to-list 'auto-mode-alist '("\\.cue\\'" . cue-mode)))

(use-package salt-mode)

(use-package nginx-mode)

;; markdown
(defun md2html (buffer)
  "Convert BUFFER of Markdown into a html string."
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
  (current-buffer)))

(defun md-preview ()
  "Preview markdown file"
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'md2html)
  (imp-visit-buffer))

(use-package simple-httpd
  :quelpa
  :config (setq httpd-port 1777))

(use-package markdown-mode
  :quelpa
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
    (setq markdown-command "pandoc -t html5"))

(use-package impatient-mode
  :quelpa)


;; go
(use-package go-mode
  :requires lsp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook 'lsp-deferred))

;; asp
(use-package pasp-mode
  :quelpa ((pasp-mode :fetcher github :repo "llaisdy/pasp-mode")))


(load-file "~/.emacs.d/init.d/markdown.el")
(load-file "~/.emacs.d/init.d/csharp.el")
(load-file "~/.emacs.d/init.d/typescript.el")
(load-file "~/.emacs.d/init.d/haskell.el")
(load-file "~/.emacs.d/init.d/rust.el")



;; (add-hook 'perl-mode-hook 'setup-perl)
;; (defun setup-perl ()
;;   "Set up 'perl-mode' key bindings etc."
;;   (progn (general-define-key
;;           :state 'normal
;;           :
;;           "K" 'cperl-perldoc-at-point))
;;   )

(general-define-key
 :keymap 'perl-mode-map
 :state 'normal
 "C-c t k" 'cperl-perldoc-at-point
 "C-c t d" 'perldb
 )



(add-hook 'find-file-hook 'infer-indents)
(defun infer-indents ()
  "Chooses tabs or spaces depending on buffer."
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (cond ((> space-count tab-count) (setq indent-tabs-mode nil))
          ((> tab-count space-count) (setq indent-tabs-mode t))
          (t (setq indent-tabs-mode nil)))))


(add-hook 'find-file-hook 'enable-line-numbers)
(defun enable-line-numbers ()
  "Turns on line numbers in current buffer."
  (display-line-numbers-mode t))


;; common
;; (global-display-line-numbers-mode t)
;; (setq linum-format "%d ")               
(setq visible-bell 1)
(setq vc-follow-symlinks nil)

(winner-mode 1)
(windmove-default-keybindings `meta)

(setq backup-directory-alist `(("." . "~/.backups")))
(setq-default tab-width 4)
(setq-default standard-indent 4)

(menu-bar-mode 0)
(tool-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

(use-package org
  :init
  (setq org-hide-emphasis-markers t)
  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  )

(use-package org-re-reveal)

(if (display-graphic-p)
  (progn
    (scroll-bar-mode 0)
    (toggle-frame-fullscreen)))

(setq tramp-default-method "ssh")

(load-file "~/.emacs.d/init.d/amigo.el")
(load-file "~/.emacs.d/init.d/server.el")
(load-file "~/.emacs.d/init.d/treemacs.el")
(load-file "~/.emacs.d/init.d/toggleWindowSplit.el")

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package gpt
  :config
  (setq gpt-openai-key "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")
  (general-define-key "C-M-g" 'gpt-dwim)
  )


(use-package eglot
  :config
  (let* (
         (start-script "/usr/local/src/PowerShellEditorServices/module/PowerShellEditorServices/Start-EditorServices.ps1")
         (module-path "/usr/local/src/PowerShellEditorServices/module")
         (log-path "/tmp/pses/emacs-test.log")
         (session-path "/tmp/pses/emacs-session.json")
         (eglot-sync-connect t))
    (add-to-list
     'eglot-server-programs
     `(powershell-mode
       . ("pwsh" "-NoLogo" "-NoProfile" "-Command" ,start-script
          "-HostName" "Emacs" "-HostProfileId" "Emacs" "-HostVersion" "1.0.0"
          "-BundledModulesPath" ,module-path
          "-LogPath" ,log-path "-LogLevel" "Diagnostic"
          "-SessionDetailsPath" ,session-path
          "-Stdio"))))
  )




(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#252520" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(font-lock-builtin-face ((t (:foreground "#E92567" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#E92672" :weight normal))))
 '(sp-show-pair-match-face ((t (:foreground "#87D700" :inverse-video nil :weight bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("9040edb21d65cef8a4a4763944304c1a6655e85aabb6e164db6d5ba4fc494a04" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default))
 '(eldoc-documentation-strategy 'eldoc-documentation-compose)
 '(evil-shift-width 2)
 '(flycheck-navigation-minimum-level 'error)
 '(global-display-line-numbers-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(line-number-mode nil)
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-image-actual-width 100)
 '(package-selected-packages
   '(evil-org json-navigator ini-mode lua-mode systemd flymake-diagnostic-at-point gpt cue-mode auto-compile face-explorer org-modern default-text-scale php-mode helm-rg org-re-reveal company helm-gtags ggtags amigo nameless names treemacs-projectile treemacs neo-tree beacon smooth-scrolling sublimity dante profile-dotemacs general evil-collection smartparens which-key quelpa-use-package quelpa impatient-mode esup shut-up go-mode projectile-ripgrep csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm evil meson-mode zenburn-theme helm-gtags ggtags amigo nameless names treemacs-projectile treemacs neo-tree beacon smooth-scrolling sublimity dante profile-dotemacs general evil-collection smartparens which-key quelpa-use-package quelpa impatient-mode esup shut-up go-mode projectile-ripgrep csv-mode magit markdown-mode powershell fuzzy-format yaml-mode helm-ag omnisharp csharp-mode helm-projectile ensime use-package monokai-theme key-chord helm))
 '(scroll-error-top-bottom t)
 '(sh-basic-offset 2)
 '(smooth-scrolling-mode t)
 '(tab-width 2))

