;;; python.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package python-mode
  :ensure-system-package
  ((python3 . "sudo apt install -y python3-full")
   (python . "sudo apt install -y python-is-python3"))
  :init
  (defun my:python-venv-new (dir &optional venv)
    "Create a new venv at DIR as VENV."
    (interactive
     (list (read-directory-name "Create new Python venv at: ")
           (intern (completing-read "`venv' directory: " nil
                                    nil nil ".venv"))))
    (let ((default-directory dir)
          (display-buffer-alist
           (list (cons "\\*Async Shell Command\\*.*"
                       (cons #'display-buffer-no-window nil)))))
      (async-shell-command (format "%s -m venv %s"
                                   (or python-shell-interpreter "python")
                                   venv))))
  (with-eval-after-load 'inheritenv
    (mapc (lambda (fn) (inheritenv-add-advice fn))
          '(run-python shell compile async-shell-command shell-command)))
  :bind
  (:map python-mode-map
   ("C-c C-." . python-indent-shift-right)
   ("C-c C-," . python-indent-shift-left)
   ;; in terminal, C-,/C-. will not be delivered
   ("C-c ." . python-indent-shift-right)
   ("C-c ," . python-indent-shift-left)
   ("C-c C-v" . my:python-venv-new))
  (:map dired-mode-map
   ("C-c C-v" . my:python-venv-new))
  :custom
  (python-indent-offset 2)
  (python-shell-interpreter "python"))

(use-package anaconda-mode
  :pin melpa
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  :custom
  (anaconda-mode-installation-directory
   (concat (file-name-directory user-init-file) ".anaconda-mode")))

(use-package company-anaconda
  :pin melpa)

(use-package buffer-env
  :pin melpa
  :custom
  (buffer-env-script-name '(".envrc" ".venv/bin/activate" "venv/bin/activate"))
  :hook
  ((hack-local-variables . buffer-env-update)))
