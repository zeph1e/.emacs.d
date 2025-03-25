(use-package web-mode
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(html\\|php\\)?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-enable-auto-opening t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-indentation t)
  (web-mode-engines-alist '(("php" . "\\.php\\'"))))
