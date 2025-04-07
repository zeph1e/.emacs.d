;;; qt.el -- Configuration for Qt C++ or Qml

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; syntax-highlighting for Qt
;; from emacswiki http://www.emacswiki.org/emacs/QtMode, slightly modified
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

;; qml-mode indentation sucks.
;; following codes are taken from my franca-idl
(defun my:qml-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (let ((curp (point))(paren 0) indent basep)
      (goto-char (min (1+ curp) (point-max))) ; for current line
      (condition-case nil
          (while (search-backward-regexp "[{}]") ; find unmatching opening brace
            (if (char-equal ?{ (char-after))
                (progn
                  (setq paren (1+ paren)) ; opening brace
                  (if (> paren 0) (error "do break")))
              ;; if } is at the first column, don't go further unless user entered it
              (if (and (/= curp (point)) (= (line-beginning-position) (point)))
                  (error "no need to go further"))
              (setq paren (1- paren))))
        (error nil))
      (if (<= paren 0) 0 ; no unmatcing open brace found
        ;; calculate indent base position
        ;;   method aa {
        ;;   _ <-- base position
        ;; We need to find another open brace in same line for the case like:
        ;;   method aa { in {
        ;;                   _ <-- indent here
        (setq basep (search-backward-regexp "[{]" (point-at-bol) t))
        (if (null basep) (setq basep (point-at-bol))
          (setq basep (1+ basep)))
        (goto-char basep)
        (skip-chars-forward "\t ")
        (setq indent (current-column)) ; candidate
        ;; We need to check if there's following statements in the same line for the case like:
        ;;   method aa { in { String aa
        ;;                    _ <-- indent here
        (goto-char basep)
        (setq basep (search-forward-regexp "[{]\\s-*[^\t {}]" (point-at-eol) t))
        (if basep
          (- basep (line-beginning-position) 1)
            (+ indent tab-width))))))

(eval-after-load "qml-mode"
  '(progn
     (defadvice qml-indent-line (around my:qml-indent-line)
       "Because the default indentation behaviour of qml-mode sucks."
       (interactive)
       (let* ((savep (point))
              (indent-col
               (save-excursion
                 (back-to-indentation)
                 (if (>= (point) savep) (setq savep nil))
                 (max (my:qml-calculate-indentation) 0))))
         (if (null indent-col) 'noindent
           (if savep
               (save-excursion (indent-line-to indent-col))
             (indent-line-to indent-col)))))
     (ad-activate 'qml-indent-line)))

(add-hook 'qml-mode-hook (lambda () (linum-mode)))

;; to switch to terrible _p.h _p_p.h headers in Qt
(defconst my:other-file-alist
  '(("_p_p\\.h\\'" (".cpp" ".cc" ".cxx"))
    ("_p\\.h\\'" ("_p_p.h" ".cpp" ".cc" ".cxx"))
    ("\\.cpp\\'" (".hpp" ".hh" ".h" "_p.h" "_p_p.h"))

    ;; original cc-find-file-alist
    ("\\.cc\\'"  (".hh" ".h"))
    ("\\.hh\\'"  (".cc" ".C"))

    ("\\.c\\'"   (".h"))
    ("\\.m\\'"   (".h"))
    ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))

    ("\\.C\\'"   (".H"  ".hh" ".h"))
    ("\\.H\\'"   (".C"  ".CC"))

    ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
    ("\\.HH\\'"  (".CC"))

    ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
    ("\\.h\\+\\+\\'" (".c++"))

    ("\\.cpp\\'" (".hpp" ".hh" ".h"))
    ("\\.hpp\\'" (".cpp"))

    ("\\.cxx\\'" (".hxx" ".hh" ".h"))
    ("\\.hxx\\'" (".cxx"))))

(eval-after-load 'find-file
  (setq-default ff-other-file-alist 'my:other-file-alist))

(provide 'utils-qt)
