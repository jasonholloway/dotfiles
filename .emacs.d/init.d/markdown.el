
;; markdown
(defun md2html (buffer)
  "Convert BUFFER of Markdown into a html string."
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))

(use-package simple-httpd
  :quelpa)

(use-package markdown-mode
  :requires impatient-mode
  :config (imp-set-user-filter 'md2html))

(setq exec-path (append exec-path '("/home/jason/.nvm/versions/node/v10.14.1/bin")))

(use-package impatient-mode
  :quelpa
  :requires simple-httpd
  :hook markdown-mode
  :config
    (httpd-start))
