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

;; LaTeX setup:
;; The idea was heavely inspired from:
;; http://emacs-fu.blogspot.com/2011/04/nice-looking-pdfs-with-org-mode-and.html
;; But I don't like to define a new custom latex class while the latex headers be easily applied to
;; writing document.

;; Download URL for using fonts (in Debian package)
;; https://packages.debian.org/sid/all/fonts-sil-gentium-basic/download
;; https://packages.debian.org/sid/all/fonts-sil-charis/download
;; https://packages.debian.org/sid/all/fonts-sil-gentium/download
;; https://packages.debian.org/sid/all/ttf-dejavu/download

(defconst my:latex-header-templates
  '(("lax" "#+LaTeX_CLASS: article\n#+LaTeX_CMD: xelatex\n#+LaTeX_HEADER: \\usepackage[T1]{fontenc}\n#+LaTeX_HEADER: \\usepackage{fontspec}\n#+LaTeX_HEADER: \\usepackage{graphicx}\n#+LaTeX_HEADER: \\usepackage{hyperref}\n#+LaTeX_HEADER: \\usepackage[hyperref,x11names]{xcolor}\n#+LaTeX_HEADER: \\usepackage[parfill]{parskip}\n#+LaTeX_HEADER: \\defaultfontfeatures{Mapping=tex-text}\n#+LaTeX_HEADER: \\setromanfont{Gentium}\n#+LaTeX_HEADER: \\setromanfont [BoldFont={Gentium Basic Bold},ItalicFont={Gentium Basic Italic}]{Gentium Basic}\n#+LaTeX_HEADER: \\setsansfont{Charis SIL}\n#+LaTeX_HEADER: \\setmonofont[Scale=.7]{DejaVu Sans Mono}\n#+LaTeX_HEADER: \\usepackage{geometry}\n#+LaTeX_HEADER: \\geometry{a4paper, textwidth=6.5in, textheight=10in,marginparsep=7pt, marginparwidth=.6in}\n#+LaTeX_HEADER: \\hypersetup{colorlinks=true,linkcolor=DodgerBlue4,citecolor=DodgerBlue4,filecolor=DodgerBlue4,urlcolor=DodgerBlue4}\n?")))

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

     (dolist (template my:latex-header-templates)
       (add-to-list 'org-structure-template-alist template))

     ;; inline images setting
     (when (display-graphic-p)
       (setq org-startup-with-inline-images t)
       (add-hook 'org-babel-after-execute-hook (lambda ()
                                                 (when org-inline-image-overlays
                                                   (org-redisplay-inline-images)))))))

(eval-after-load 'ox-latex
  '(progn
     (setq org-latex-tables-centered t)
     (setq org-latex-tables-booktabs t)
     ;; (setq org-latex-pdf-process '("latexmk -pdf -quiet %f"))
     ;; (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
     ;;                               "xelatex -interaction nonstopmode %f")) ; for multiple passes
     ))

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
     (defadvice org-reveal-export-to-html
         (around org-reveal-export-to-html-and-copy-reveal activate compile)

       ;; get reveal.js submodule if it wasn't fetched
       (unless (directory-files reveal-local-repository nil
                                "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)") ; empty
         (message "Reveal.js is not yet fetched. Fetching...")
         (let ((default-directory user-emacs-directory)
               (git-cmdargs '(("submodule" "init")
                              ("submodule" "update"))))
           (dolist (args git-cmdargs)
             (unless (zerop (apply #'call-process
                                   (append '("git" nil nil nil) args)))
               (error "Git command failed with args: %S" args)))
           (message "Reveal.js fetched.")))

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
