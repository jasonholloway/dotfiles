(require `package)
;;(setq package-check-signature nil)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;;(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package quelpa
  :demand t
  :init
  (setq quelpa-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher github
       :repo "quelpa/quelpa-use-package")))
  (require 'quelpa-use-package))
