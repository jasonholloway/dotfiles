
(use-package general)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-move-beyond-eol t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package which-key
  :custom
  (which-key-idle-delay 0.7)
  :config
  (which-key-mode)
  :bind (("C-h h" . 'which-key-show-top-level)))
