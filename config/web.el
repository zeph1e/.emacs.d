(use-package web-mode
  :commands web-mode
  :mode "\\.\\(html\\|php\\)?\\'"
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

(use-package web-beautify
  :init
  (eval-after-load 'js
    '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode
    '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'sgml-mode
    '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'web-mode
    '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
  (eval-after-load 'css-mode
    '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))

  :config
  (setq web-beautify-args '("-s" "2" "-f" "-"))
  :ensure-system-package
  ((npm . "sudo apt install -y npm")
   ("~/.local/lib/node_modules" . "npm config set prefix ~/.local")
   (js-beautify . "npm -g install js-beautify")))

(use-package js
  :pin manual
  :ensure-system-package
  ((npm . "sudo apt install -y npm")
   ("~/.local/lib/node_modules" . "npm config set prefix ~/.local")
   (typescript-language-server . "npm -g install typescript-language-server")
   (tsc . "npm -g install typescript"))
  :custom
  (js-indent-level 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :ensure-system-package
  ((npm . "sudo apt install -y npm")
   ("~/.local/lib/node_modules" . "npm config set prefix ~/.local")
   (typescript-language-server . "npm -g install typescript-language-server")
   (tsc . "npm -g install typescript"))
  :custom
  (typescript-indent-level 2))

(use-package css-mode
  :custom
  (css-indent-offset 2))
