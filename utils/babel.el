;;; babel.el -- org-babel configuration

(defconst babel-language-alist '((awk . t)
                                 (C . t)
                                 (cpp . t)
                                 (css . t)
                                 (emacs-lisp . t)
                                 (gnuplot . t)
                                 (java . t)
                                 (python . t)
                                 (plantuml . t)
                                 (sh . t)))

(eval-after-load 'org
  '(progn
     (require 'ob-awk)
     (require 'ob-C)
     (require 'ob-css)
     (require 'ob-emacs-lisp)
     (require 'ob-gnuplot)
     (require 'ob-java)
     (require 'ob-python)
     (require 'ob-plantuml) (setq org-plantuml-jar-path "~/.emacs.d/el-get/plantuml-mode/plantuml.jar")
     (require 'ob-sh)
     (org-babel-do-load-languages 'org-babel-do-load-languages babel-language-alist)
     (setq org-confirm-babel-evaluate nil)))
