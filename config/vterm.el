(use-package vterm
  :init
  ;; Workaround to install system dependencies of vterm
  ;; vterm-module cannot be installed using `:ensure-system-package'.
  (defvar my:vterm-system-deps
    '((gcc . build-essential) (cmake . cmake)(libtool . libtool-bin)))

  (defun my:vterm-dependencies-checker (&optional cmd)
    (when (stringp cmd)
      (let ((pkg (intern (car (last (split-string cmd))))))
        (setq my:vterm-system-deps
              (cl-remove-if #'(lambda (dep) (equal (cdr dep) pkg))
                            my:vterm-system-deps))))
    (unless my:vterm-system-deps
      (exit-recursive-edit))) ; resume vterm-module-compile


  (defun my:vterm-module-compile (orig-fun &rest args)
    (interactive)
    (unless (memq 'my:vterm-dependencies-checker upesp+:command-executed-hook)
      (setq my:vterm-module-compile-orig-fun orig-fun)
      (add-hook 'upesp+:command-executed-hook 'my:vterm-dependencies-checker)
      (run-with-timer 0 nil #'my:vterm-module-compile-internal)
      (recursive-edit) ; block here and wait for installation to be done
      (funcall orig-fun)
      (remove-hook 'upesp+:command-executed-hook 'my:vterm-dependencies-checker)))

  (defun my:vterm-module-compile-internal ()
    (dolist (deps my:vterm-system-deps)
      (let ((cmd (symbol-name (car deps)))
            (pkg (symbol-name (cdr deps))))
        (if (executable-find cmd)
            (setq my:vterm-system-deps (cl-remove deps my:vterm-system-deps))
          (upesp+:async-shell-command (concat "sudo apt install -y " pkg)))))
    (my:vterm-dependencies-checker))


  (advice-add 'vterm-module-compile :around #'my:vterm-module-compile)
  :bind
  (:map my:global-key-map
   ("C-x t" . vterm)))
