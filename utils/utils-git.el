;;; utils-git.el

(when (eq system-type 'windows-nt) ; to resolve push problems in windows
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(eval-after-load 'vc-git
  '(defun vc-git--rev-parse (rev)
     (with-temp-buffer
       (and
        (vc-git--out-ok "rev-parse" "--short" rev)
        (buffer-substring-no-properties (point-min) (1- (point-max)))))))

(defadvice magit-ediff-resolve (after my:magit-ediff-resolve (file))
  "Advice function for magit-ediff-resolve to disable read only."
  (magit-with-toplevel
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (setq-local buffer-read-only nil)))))
(ad-activate 'magit-ediff-resolve)

(provide 'utils-git)
