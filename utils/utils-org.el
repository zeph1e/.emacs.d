;;; utils-org.el -- org-babel configuration

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
     (require 'utils-theme)
     (setq org-html-head
           (concat "<style type=\"text/css\">"
                   "pre, pre.src { background-color: " (face-attribute 'default :background) "; "
                   "color: " (face-attribute 'default :foreground) "; }</style>"))
     (setq org-startup-truncated nil)
     (setq org-startup-folded nil)

     ;; inline images setting
     (when (display-graphic-p)
       (setq org-startup-with-inline-images t)
       (add-hook 'org-babel-after-execute-hook (lambda ()
                                                 (when org-inline-image-overlays
                                                   (org-redisplay-inline-images)))))))

;; org-present
(eval-after-load 'org-present
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (unless org-startup-with-inline-images
                   (org-display-inline-images))
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (unless org-startup-with-inline-images
                   (org-remove-inline-images))
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; org-reveal
(defconst reveal-local-repository "~/.emacs.d/misc/reveal.js")
(eval-after-load 'ox-reveal
  '(progn
     (setq org-reveal-root "./.reveal.js")
     (defadvice org-reveal-export-to-html (around org-reveal-export-to-html-and-copy-reveal activate compile)
         (let* ((file-name ad-do-it)
                (file-full-name (expand-file-name file-name))
                (reveal-dir-name (concat (file-name-directory file-full-name) ".reveal.js/")))
           (unless (file-exists-p reveal-dir-name)
             (message "Copying reveal.js to target directory...")
             (copy-directory reveal-local-repository reveal-dir-name))
           file-name))))
(when (el-get-package-is-installed 'org-reveal)
  (defun org-reveal-update-reveal-js ()
    (interactive)
    (let* ((default-directory "~/.emacs.d"))
      (magit-submodule-update t))))

(provide 'utils-org)
