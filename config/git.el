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

(use-package magit-gerrit
  :ensure nil
  :pin manual
  :config
  (setq-default magit-gerrit-push-to "for")
  :custom
  (magit-gerrit-known-hosts '("[a-zA-Z0-9]+.lge.com"
                              "[a-zA-Z0-9]+.lgsvl.com"))
  (magit-gerrit-popup-prefix (kbd "`")))
