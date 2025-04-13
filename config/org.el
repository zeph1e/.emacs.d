;;; org.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package org
  :pin gnu
  :config
  ;; Babel language support configuration
  ;; See https://orgmode.org/worg/org-contrib/babel/languages/index.html
  ;; :lang : The language keyword right after #+BEGIN_SRC
  ;; :req  : The required ob-*, language specific extension to give to
  ;;         org-babel-load-languages. When there's no :req specified, it uses
  ;;         the value of :lang instead.
  ;; :mode : The mode name to edit the code block in.  When there's no :mode
  ;;         specified, it uses the value of :lang instead.
  ;; :deps : The executable name to parse or compile the language
  ;; :settings : The things to evaluate
  (defconst babel-language-plist
    '((:lang awk :deps "awk")
      (:lang C :mode c :deps "gcc")
      (:lang C++ :req C :mode c++ :deps "g++")
      (:lang css)
      (:lang emacs-lisp)
      (:lang elisp :req emacs-lisp :mode emacs-lisp)
      (:lang gnuplot :deps "gnuplot")
      (:lang java :deps "java")
      (:lang org)
      (:lang python :deps "python")
      (:lang plantuml
       :settings
        (progn (require 'plantuml-mode)
               (setq org-plantuml-jar-path plantuml-jar-path)))
      (:lang ruby :deps "ruby")
      (:lang shell :mode shell-script)))

    (let ((load-languages))
      (dolist (prop babel-language-plist)
        (let* ((lang (plist-get prop :lang))
               (req (or (plist-get prop :req) lang))
               (mode (or (plist-get prop :mode) lang))
               (deps (or (plist-get prop :deps)))
               (settings (plist-get prop :settings)))
          (when (null (alist-get req load-languages))
            (push (cons req (if deps (and (executable-find deps) t) t))
                  load-languages))
          (add-to-list 'org-src-lang-modes (cons (symbol-name lang) mode))
          (eval settings))
      (org-babel-do-load-languages
       'org-babel-load-languages load-languages)))
    :after (plantuml-mode))

;; Required for html export
(use-package htmlize)
