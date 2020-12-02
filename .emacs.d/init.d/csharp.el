
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
;;   (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;   (add-hook 'csharp-mode-hook 'company-mode)
;;   (add-hook 'csharp-mode-hook 'flycheck-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete))

