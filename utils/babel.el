;;; babel.el -- org-babel configuration

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(defconst babel-language-alist '((:lang awk :req ob-awk :mode awk :deps "awk")
                                 (:lang C :req ob-C :mode c :deps "gcc")
                                 (:lang cpp :req ob-C :mode c++ :deps "g++")
                                 (:lang css :req ob-css :mode css)
                                 (:lang emacs-lisp :req ob-emacs-lisp :mode emacs-lisp)
                                 (:lang gnuplot :req ob-gnuplot :mode gnuplot :deps "gnuplot")
                                 (:lang java :req ob-java :mode java :deps "java")
                                 (:lang org :req ob-org :mode org)
                                 (:lang python :req ob-python :mode python :deps "python")
                                 (:lang plantuml
                                        :req ob-plantuml
                                        :mode plantuml
                                        :settings (setq org-plantuml-jar-path
                                                        "~/.emacs.d/el-get/plantuml-mode/plantuml.jar"))
                                 (:lang ruby :req ob-ruby :mode ruby :deps "ruby")
                                 (:lang sh :req ob-sh :mode shell-script)))

(eval-after-load 'org
  '(progn
     (let (load-languages)
       (dolist (lang babel-language-alist)
         (when (and lang (if (plist-get lang ':deps) (executable-find (plist-get lang ':deps)) t))
           (push (cons (plist-get lang ':lang) t) load-languages)
           (require (plist-get lang ':req))
           (add-to-list 'org-src-lang-modes
                        (cons (symbol-name (plist-get lang ':lang)) (plist-get lang ':mode)))
           (eval (plist-get lang ':settings))))
       (org-babel-do-load-languages 'org-babel-do-load-languages load-languages))

     ;; other settings
     (setq org-confirm-babel-evaluate nil) ; turn off annoying prompt

     ;; inline images setting
     (when (display-graphic-p)
       (setq org-startup-with-inline-images t)
       (add-hook 'org-babel-after-execute-hook (lambda ()
                                                 (when org-inline-image-overlays
                                                   (org-redisplay-inline-images)))))))

