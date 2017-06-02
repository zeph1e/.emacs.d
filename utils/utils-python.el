;; utils-python.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; rather to use python3.x
(setq python-shell-interpreter (or (executable-find "python3")
                                   "python"))

;; anaconda-mode settings

;; fix installation directory
(setq anaconda-mode-installation-directory
      (el-get-package-directory 'anaconda-mode))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; A workaround for emacs bug in python-shell-completion-native-try (fixed in 25.2rc)
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25753#44
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(provide 'utils-python)
