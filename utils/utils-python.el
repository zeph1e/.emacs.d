;; utils-python.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

;; rather to use python3.x
(setq python-shell-interpreter
      (or (executable-find "python3") "python"))

;; anaconda-mode settings

;; fix installation directory
(setq anaconda-mode-installation-directory
      (el-get-package-directory 'anaconda-mode))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(defadvice python-shell-send-buffer (after my:python-shell-send-buffer)
  "Advice function for `python-shell-send-buffer' to show python buffer."
  (interactive)
  (let ((buffer-name (format "*%s*" python-shell-buffer-name)))
    (when (null (get-buffer-window buffer-name))
      (display-buffer buffer-name))))
(ad-activate 'python-shell-send-buffer)

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

;; python key map settings
(with-eval-after-load 'python
  (let ((map python-mode-map))
    (define-key map (kbd "C-c C-.") 'python-indent-shift-right)
    (define-key map (kbd "C-c C-,") 'python-indent-shift-left)
    ;; in term, C-,/C-. will not be delivered
    (define-key map (kbd "C-c .") 'python-indent-shift-right)
    (define-key map (kbd "C-c ,") 'python-indent-shift-left)))

(provide 'utils-python)
