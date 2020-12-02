
;; Treemacs
(use-package treemacs
  :hook (treemacs-mode . jh/treemacs-hook)
  :init
  (setq
   treemacs-move-forward-on-expand t
   treemacs-show-cursor nil
   )
  (general-def 'normal
    "M-'" 'treemacs)
  (defun jh/treemacs-hook ()
    (display-line-numbers-mode -1)
    (linum-mode -1))
  (advice-add 'treemacs-visit-node-no-split
              :after (lambda (_) (treemacs))))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  (general-define-key
   :states 'treemacs
   :keymaps 'treemacs-mode-map
    "M-'" 'treemacs
    "M-j" 'treemacs-next-neighbour
    "M-k" 'treemacs-previous-neighbour
    "h"   'treemacs-collapse-parent-node
    "M-h" 'treemacs-goto-parent-node
    "M-l" 'treemacs-TAB-action
    "l"   'treemacs-RET-action
    "p"   'treemacs-peek
    "b"   'treemacs-peek
    "H"   'treemacs-root-up
    "L"   'treemacs-root-down))

(use-package treemacs-projectile
  :after (treemacs projectile))
