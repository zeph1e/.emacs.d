;;; utils-git.el

(when (eq system-type 'windows-nt) ; to resolve push problems in windows
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(provide 'utils-git)
