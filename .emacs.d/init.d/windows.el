
(defvar is-windows
  (member system-type '(windows-nt cygwin)))

(if is-windows
  (setq gc-cons-threshold 100000000))

(if is-windows
  (setq vc-handled-backends nil))

(if is-windows
		(add-hook 'window-setup-hook 'toggle-frame-fullscreen t))
