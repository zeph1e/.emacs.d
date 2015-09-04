;;; present.el -- presentation settings

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; org-present
(eval-after-load 'org-present
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (unless org-startup-with-inline-images
                   (org-display-inline-images))
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (unless org-startup-with-inline-images
                   (org-remove-inline-images))
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;; org-reveal
(defconst reveal-local-repository "~/.emacs.d/misc/reveal.js")
(eval-after-load 'ox-reveal
  '(progn
     (setq org-reveal-root "./.reveal.js")
     (defadvice org-reveal-export-to-html (around org-reveal-export-to-html-and-copy-reveal activate compile)
         (let* ((file-name ad-do-it)
                (file-full-name (expand-file-name file-name))
                (reveal-dir-name (concat (file-name-directory file-full-name) ".reveal.js/")))
           (unless (file-exists-p reveal-dir-name)
             (message "Copying reveal.js to target directory...")
             (copy-directory reveal-local-repository reveal-dir-name))
           file-name))))
(defun org-reveal-update-reveal-js ()
       (interactive)
       (let* ((default-directory "~/.emacs.d"))
         (magit-submodule-update t)))
