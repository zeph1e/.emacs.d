;;; babel.el -- org-babel configuration

(defconst babel-language-alist '((awk . ob-awk)
                                 (C . ob-C)
                                 (cpp . ob-C)
                                 (css . ob-css)
                                 (emacs-lisp . ob-emacs-lisp)
                                 (gnuplot . ob-gnuplot)
                                 (java . ob-java)
                                 (python . ob-python)
                                 (plantuml . ob-plantuml)
                                 (sh . ob-sh)))

(eval-after-load 'org
  '(progn
     (let (load-languages require-list)
       (dolist (lang babel-language-alist)
         (push (cons (car lang) t) load-languages)
         (add-to-list 'require-list (cdr lang)))
       (dolist (req require-list)
         (require req))
       (org-babel-do-load-languages 'org-babel-do-load-languages load-languages))
     (org-babel-do-load-languages 'org-babel-do-load-languages babel-language-alist)

     ;; additional settings for each languages
     (setq org-plantuml-jar-path "~/.emacs.d/el-get/plantuml-mode/plantuml.jar")

     ;; other settings
     (setq org-confirm-babel-evaluate nil) ; turn off annoying prompt

     ;; inline images setting
     (when (display-graphic-p)
       (setq org-startup-with-inline-images t)
       (add-hook 'org-babel-after-execute-hook (lambda ()
                                                 (when org-inline-image-overlays
                                                   (org-redisplay-inline-images)))))))

