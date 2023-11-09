
(use-package general)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-move-beyond-eol t)
  :config
  (evil-mode 1)
  (global-set-key (kbd "M-SPC") 'evil-window-next)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  )

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
