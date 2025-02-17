;;-*- mode: emacs-lisp; -*-
(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  :init
  (setq-default
   c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (python-mode . "python")
                     (c-mode . "google"))
   indent-tabs-mode nil ; don't insert tabs in indent
   tab-always-indent nil))


(use-package google-translate
  :init
  (eval-after-load 'google-translate-default-ui
    '(progn
       (setq google-translate-default-source-language "auto"
             google-translate-default-target-language "ko")))
  (eval-after-load 'google-translate-smooth-ui
    '(progn
       (setq google-translate-translation-directions-alist
             '(("en" . "ko") ("ko" . "en") ("de" . "ko") ("ko" . "de")
               ("en" . "de") ("de" . "en") ("ko" . "ja")))))
  :bind
  (:map my:global-key-map
   ("M-+" . google-translate-at-point)
   ("C-+" . google-translate-smooth-translate)))
