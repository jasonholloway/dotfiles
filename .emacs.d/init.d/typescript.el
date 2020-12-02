
;; typescript stuff
(defun setup-tide ()
  "Setup Tide for any mode that does typescripty things."
  (tide-setup)
  (flycheck-mode 1)
  (company-mode 1)
  (tide-hl-identifier-mode +1)
  (general-define-key
   :states 'normal
   :keymaps 'local
   :prefix "C-c t"
   "f" 'tide-fix
   "F" 'tide-refactor
   "r" 'tide-rename-symbol
   "R" 'tide-rename-file
   "e" 'tide-error-at-point
   "E" 'tide-project-errors
   "g" 'tide-jump-to-definition
   "g" 'tide-jump-to-implementation
   "p" 'tide-format)
  (general-define-key
   :states 'normal
   :keymaps 'local
   "M-n" 'flycheck-next-error
   "M-p" 'flycheck-previous-error
   "M-l" 'flycheck-list-errors
   ))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . setup-tide)
         (web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (setup-tide))))))

(use-package web-mode
  :after flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White"))
