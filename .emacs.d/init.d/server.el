
(defadvice server-ensure-safe-dir
    (around
     my-around-server-ensure-safe-dir
     activate)
  "suppresses fatal error when server dir unsafe (on Windows unavoidable)"
  (ignore-errors ad-do-it))

(setq server-socket-dir "~/.emacs.d/server/")
(setq server-auth-dir "~/.emacs.d/server/")
