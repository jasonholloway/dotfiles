
(defvar rust-src-path "/home/jason/.rustup/toolchains/nightly-x86_64-pc-windows-gnu/lib/rustlib/src/rust/library")

(use-package rustic
  :ensure
  :after lsp-mode
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-racer-rust-src-path rust-src-path)
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

;; (use-package racer
;;   :ensure
;;   :custom
;;   (racer-rust-src-path rust-src-path))

(use-package lsp-ui
  :ensure
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "M-l"))
                        (lsp-enable-which-key-integration))))
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-eldoc-render-all t)
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
