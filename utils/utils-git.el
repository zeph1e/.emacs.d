;;; utils-git.el

(when (eq system-type 'windows-nt) ; to resolve push problems in windows
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(eval-after-load 'vc-git
  '(defun vc-git--rev-parse (rev)
     (with-temp-buffer
       (and
        (vc-git--out-ok "rev-parse" "--short" rev)
        (buffer-substring-no-properties (point-min) (1- (point-max)))))))

(provide 'utils-git)
