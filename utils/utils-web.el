;;; utils-web.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(eval-after-load 'web-mode
  `(setq web-mode-markup-indent-offset 2
         web-mode-attr-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-sql-indent-offset 2

         web-mode-enable-auto-opening t
         web-mode-enable-auto-closing t
         web-mode-enable-auto-pairting t
         web-mode-enable-auto-indentation t
         ))

(provide 'utils-web)
