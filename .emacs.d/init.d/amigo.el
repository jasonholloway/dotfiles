
(use-package amigo
  :quelpa (amigo
           ;; :fetcher github :repo jasonholloway/amigo)
           :fetcher file :path "~/src/el/amigo")
  :init
  (amigo-specify 'term
                 '((params . ((side . right)))
                   (get-context . (lambda () "Blah"))
                   (get-buffer . (lambda () (make-term "blah" "/bin/bash")))))
  (amigo-specify 'todo
                 '((params . ((side . right)))
                   (get-context . (lambda () "todo"))
                   (get-buffer . (lambda () (generate-new-buffer "todo")))))
  (general-define-key
   "M-#" (lambda () (interactive) (amigo-toggle 'term))
   "M-]" (lambda () (interactive) (amigo-toggle 'todo))))
