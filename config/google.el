;;-*- mode: emacs-lisp; -*-
(use-package google-c-style
  :config
  (setf (alist-get 'c++-mode c-default-style) "google"
        (alist-get 'c-mode c-default-style) "google")
  :hook
  (c-mode-common . google-set-c-style))

(use-package google-translate
  :init
  (eval-after-load 'google-translate-smooth-ui
    '(progn
       (setq google-translate-translation-directions-alist
             '(("en" . "ko") ("ko" . "en") ("de" . "ko") ("ko" . "de")
               ("en" . "de") ("de" . "en") ("ja" . "ko") ("ko" . "ja")))
       ;; Detect source language and select preferred target language
       (defconst my:google-translate-unicode-range
         '(("ko" . ((#xAC00 . #xD3A3)))
           ("ja" . ((#x3041 . #x30F6)))
           ("de" . (#x00C4 #x00D6 #x00DC #x00DF #x00E4 #x00F6 #x00FC))))

       (defun my:google-translate-translate (args)
         (let ((source-lang (nth 0 args))
               (target-lang (nth 1 args))
               (text (nth 2 args)))
           (catch 'break
             (dotimes (l (length my:google-translate-unicode-range))
               (let ((lang (car (nth l my:google-translate-unicode-range)))
                     (range (cdr (nth l my:google-translate-unicode-range))))
                 (dotimes (i (length text))
                   (let ((c (aref text i)))
                     (when (mapcan (lambda (elem)
                                     (cond ((numberp elem)
                                            (= elem c))
                                           ((listp elem)
                                            (and (<= (car elem) c)
                                                 (>= (cdr elem) c)))
                                           (t nil)))
                                   range)
                       (setq source-lang lang)
                       (throw 'break nil)))))))
           (when (or (string= source-lang target-lang)
                     (string= target-lang "auto"))
             (catch 'break
               (dotimes (i (length google-translate-translation-directions-alist))
                 (let ((dir (nth i google-translate-translation-directions-alist)))
                   (when (string= (car dir) source-lang)
                     (setq target-lang (cdr dir))
                     (throw 'break nil))))))
           (setf (nth 0 args) source-lang
                 (nth 1 args) target-lang)
           args))

       (advice-add 'google-translate-translate
                   :filter-args 'my:google-translate-translate)))
  :bind
  (:map my:global-key-map
   ("M-+" . google-translate-at-point)
   ("C-+" . google-translate-smooth-translate))
  :custom
  (google-translate-default-source-language "auto")
  (google-translate-default-target-language "ko"))
