(use-package vterm
  :init
  ;; Workaround to install system dependencies of vterm
  ;; vterm-module cannot be installed using `:ensure-system-package'.
  (defvar my:vterm-system-deps
    '((gcc . build-essential) (cmake . cmake)(libtool . libtool-bin)))

  (defun my:vterm-dependencies-checker (&optional cmd)
    "Drop the package installed by CMD from `my:vterm-system-deps'.
Exit the recursive edit started by `my:vterm-module-compile' once every
dependency in `my:vterm-system-deps' has been installed."
    (when (stringp cmd)
      (let ((pkg (intern (car (last (split-string cmd))))))
        (setq my:vterm-system-deps
              (cl-remove-if #'(lambda (dep) (equal (cdr dep) pkg))
                            my:vterm-system-deps))))
    (unless my:vterm-system-deps
      (exit-recursive-edit))) ; resume vterm-module-compile


  (defun my:vterm-module-compile (orig-fun &rest args)
    "Around-advice for `vterm-module-compile' (ORIG-FUN with ARGS).
Blocks until the system packages listed in `my:vterm-system-deps' have
finished installing, then runs the original compile."
    (interactive)
    (unless (memq 'my:vterm-dependencies-checker upesp+:command-executed-hook)
      (setq my:vterm-module-compile-orig-fun orig-fun)
      (add-hook 'upesp+:command-executed-hook 'my:vterm-dependencies-checker)
      (run-with-timer 0 nil #'my:vterm-module-compile-internal)
      (recursive-edit) ; block here and wait for installation to be done
      (funcall orig-fun)
      (remove-hook 'upesp+:command-executed-hook 'my:vterm-dependencies-checker)))

  (defun my:vterm-module-compile-internal ()
    "Install each missing system dependency listed in `my:vterm-system-deps'.
Already-available commands are dropped from the list directly; missing
ones are queued through `upesp+:async-shell-command'."
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
