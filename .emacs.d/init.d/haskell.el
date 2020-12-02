
;; haskell stuff
(use-package haskell-mode
  :config
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(use-package hindent
  :after haskell-mode
  :config
    (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package attrap)

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint)))

;; (use-package intero
;;   :after haskell-mode
;;   :config
;;     (add-hook 'haskell-mode-hook 'intero-mode))

(use-package helm-hoogle
  :after haskell-mode)


(use-package dash
  :init (dash-enable-font-lock))

(use-package helm-gtags
  :hook (('typescript-mode . 'helm-gtags-mode))
  :config
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t)
  (general-def 'helm-gtags-mode-map
    "M-t" 'helm-gtags-find-tag)
  
  ;; (with-eval-after-load 'helm-gtags
  ;;   (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  ;;   (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  ;;   (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  ;;   (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  ;;   (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  ;;   (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  ;;   (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))
  )
