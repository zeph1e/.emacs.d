;;; dir-locals.el

((nil . ((fill-column . 80)))
 (emacs-lisp-mode . ((eval . (ignore-errors
                               (add-hook 'write-contents-functions
                                         #'(lambda ()
                                             (delete-trailing-whitespace)
                                             nil))))
                     (buffer-read-only . t))))
