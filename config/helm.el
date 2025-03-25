;;-*- mode: emacs-lisp; -*-
(use-package helm
  :demand t
  :config
  (require 'helm-files)
  (defun my:helm-find-files-backward-dwim ()
    "Backspace goes to the upper folder if you're not in a filename."
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-backward-char 1)))
  :bind
  (:map my:global-key-map
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring)
   ("C-x C-f"   . helm-find-files)
   ("C-x r b"   . helm-filtered-bookmarks)
   ("C-x r i"   . helm-register)
   ("C-x b"     . helm-mini)
   ("C-h a"     . helm-apropos)
   ("M-r"       . helm-occur)
   ("M-R"       . helm-grep-do-git-grep)
   ("M-g s"     . helm-google-suggest)
   ("C-x C-SPC" . helm-all-mark-rings)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-j"   . helm-select-action)
   ("M-n"   . helm-next-source)
   ("M-p"   . helm-previous-source)
   :map helm-read-file-map
   ("<backspace>" . my:helm-find-files-backward-dwim)
   :map helm-find-files-map
   ("<backspace>" . my:helm-find-files-backward-dwim))
  :custom
  (helm-split-window-preferred-function 'helm-split-window-default-fn)
  (helm-autoresize-mode 1)
  (helm-move-to-line-cycle-in-source nil)
  (helm-ff-search-library-in-sexp t)
  (helm-scroll-amount 8)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-auto-update-initial-value nil)
  (helm-M-x-fuzzy-match t)
  (helm-split-window-in-side-p t)
  (helm-bookmark-default-filtered-sources '(helm-source-bookmark-files&dirs
					    helm-source-bookmark-helm-find-files
					    helm-source-bookmark-info
					    helm-source-bookmark-gnus
					    helm-source-bookmark-man
					    helm-source-bookmark-images
					    helm-source-bookmark-w3m
					    helm-source-bookmark-uncategorized)))

(use-package helm-projectile
  :bind-keymap
  (("C-x C-p" . projectile-command-map)
   ("C-x p"   . projectile-command-map))
  :init
  (helm-projectile-on)
  :after (helm projectile))

(use-package helm-ag
  :bind
  (:map helm-ag-map
   ("M-n" . helm-ag--next-file)
   ("M-p" . helm-ag--previous-file))
  (:map my:global-key-map
   ("C-M-r" . helm-ag))
  :custom
  (helm-ag-insert-at-point 'thing-at-point)
  (helm-ag-fuzzy-match t)
  :after (helm))

(use-package helm-descbinds
  :commands helm-descbinds
  :bind
  (:map my:global-key-map
   ("C-h b" . helm-descbinds))
  :custom
  helm-descbinds-mode t
  :after (helm))
