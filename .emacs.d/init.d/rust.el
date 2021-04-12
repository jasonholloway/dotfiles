
(defvar rust-src-path "/home/jason/.rustup/toolchains/nightly-x86_64-pc-windows-gnu/lib/rustlib/src/rust/library")

(use-package rustic
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
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-hide t)
  (lsp-ui-sideline-toggle-symbols-info nil)
  (lsp-ui-peek-always-show t))

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "M-l"))
                  (lsp-enable-which-key-integration))))
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-eldoc-render-all nil)
  :init
  (setq lsp-rust-all-targets t)
  (setq lsp-rust-analyzer-cargo-all-targets t)
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
