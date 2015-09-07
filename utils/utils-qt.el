;;; qt.el -- from emacswiki http://www.emacswiki.org/emacs/QtMode, slightly modified
;; syntax-highlighting for Qt
;; (based on work by Arndt Gulbrandsen, Troll Tech)
(defun jk/c-mode-common-hook ()
  "Set up c-mode and related modes.

 Includes support for Qt code (signal, slots and alikes)."

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords (optimized)
  (setq c-protection-key (concat "\\<\\(?:p\\(?:r\\(?:ivate\\(?: \\(?:Q_SLOT\\|slot\\)\\)"
                                 "?\\|otected\\(?: \\(?:Q_SLOT\\|slot\\)\\)?\\)\\|ublic\\"
                                 "(?: \\(?:Q_SLOT\\|slot\\)\\)?\\)\\)\\>")
        c-C++-access-key (concat "\\<\\(?:p\\(?:r\\(?:ivate\\(?: \\(?:Q_SLOTS\\|slots\\)\\)"
                                 "?\\|otected\\(?: \\(?:Q_SLOTS\\|slots\\)\\)?\\)\\|ublic\\("
                                 "?: \\(?:Q_SLOTS\\|slots\\)\\)?\\)\\|signals\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<\\(uchar\\|ushort\\|uint\\|qlonglong\\|qulonglong\\|QString\\)\\>"
                               . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face (face-foreground 'font-lock-keyword-face))
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
                            '(("\\<Q_[A-Z]+\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
                            '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    ))
(add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)
