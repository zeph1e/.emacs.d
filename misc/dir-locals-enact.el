((nil . ((buffer-read-only . t)
         (tab-width . 4)
         (indent-tabs-mode . t)
         (js-indent-level . 4)
         (css-indent-offset . 4)
         (eval . (setq-local projectile-project-root
                             (let ((root (dir-locals-find-file
                                          dir-locals-file)))
                               (cond
                                ((listp root) (car root))
                                ((stringp root) root)
                                (t nil)))))))
 (lisp-data-mode . ((indent-tabs-mode . nil)))
 (auto-mode-alist . (("\\.js\\'" . js-jsx-mode))))
