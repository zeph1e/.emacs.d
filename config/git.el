;;; git.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package magit
  :bind
  (:map my:global-key-map
   ("C-x RET C-s" . magit)
   ("C-x RET C-h" . magit-log-head)
   ("C-x RET C-b" . magit-blame)
   ("C-x RET C-f" . magit-find-file)
   ("C-x RET C-l" . magit-log-buffer-file))
  :hook
  (text-mode . (lambda ()
                 (let ((file-name (buffer-file-name)))
                   (when file-name
                     (when (string-match ".+\\(.git/COMMIT_EDITMSG\\)\\'"
                                         file-name)
                       (setq-local fill-column 70)
                       (display-fill-column-indicator-mode 1)))))))

(use-package magit-gh
  :after magit)
