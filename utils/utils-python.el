;; utils-python.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(setq anaconda-mode-installation-directory
      (el-get-package-directory 'anaconda-mode))
(setq python-shell-interpreter (or (executable-find "python3")
                                   "python"))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(provide 'utils-python)
