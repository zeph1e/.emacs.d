;;-*- mode: emacs-lisp; -*-
(use-package projectile
  :init
  (projectile-global-mode)
  :custom
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire (* 7 24 60 60)) ; a week
  (projectile-file-exists-local-cache-expire (* 7 24 60 60)) ; a week
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'projectile-find-file-dwim)
  (projectile-project-search-path '(("~/Workspace" . 1))))
