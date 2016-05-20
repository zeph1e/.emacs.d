;; utils-helm.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(let ((map helm-map))
  (define-key map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key map (kbd "C-j") 'helm-select-action)
  (define-key map (kbd "M-f") 'helm-next-source)
  (define-key map (kbd "M-b") 'helm-previous-source))

(setq helm-split-window-preferred-function 'helm-split-window-default-fn
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t
      helm-M-x-fuzzy-match t
      helm-split-window-in-side-p t

      ;; projectile
      projectile-enable-caching t
      projectile-file-exists-remote-cache-expire (* 10 60)
      projectile-completion-system 'helm
      projectile-switch-project 'helm-projectile)

;; replace projectile prefix key
(let ((map projectile-mode-map))
  (define-key map projectile-keymap-prefix nil)
  (define-key map (kbd "C-x p") 'projectile-command-map))

(projectile-global-mode)
(helm-autoresize-mode 1)
(helm-projectile-on)

;; http://emacs.stackexchange.com/questions/2563/helm-search-within-buffer-feature
(defconst my:helm-follow-sources
  '(helm-source-occur
    helm-source-grep)
  "List of sources for which helm-follow-mode should be enabled")

(defun my:helm-set-follow ()
  "Enable helm-follow-mode for the sources specified in the list
variable `my-helm-follow-sources'. This function is meant to
be run during `helm-initialize' and should be added to the hook
`helm-before-initialize-hook'."
  (mapc (lambda (source)
          (when (memq source my:helm-follow-sources)
            (helm-attrset 'follow 1 (symbol-value source))))
        helm-sources))

;; Add hook to enable helm-follow mode for specified helm
(add-hook 'helm-before-initialize-hook 'my:helm-set-follow)

(provide 'utils-helm)
