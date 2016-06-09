;; utils-helm.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'helm-ag)

(let ((map helm-map))
  (define-key map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key map (kbd "C-j") 'helm-select-action)
  (define-key map (kbd "M-j") 'helm-next-source)
  (define-key map (kbd "M-k") 'helm-previous-source))

(let ((map helm-ag-map))
  (define-key map (kbd "M-j") 'helm-ag--next-file)
  (define-key map (kbd "M-k") 'helm-ag--previous-file))

(setq helm-split-window-preferred-function 'helm-split-window-default-fn
      helm-move-to-line-cycle-in-source nil
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t
      helm-ff-auto-update-initial-value nil
      helm-M-x-fuzzy-match t
      helm-split-window-in-side-p t

      ;; helm-ag
      helm-ag-insert-at-point 'thing-at-point
      helm-ag-fuzzy-match t

      ;; projectile
      projectile-enable-caching t
      projectile-file-exists-remote-cache-expire (* 7 24 60 60) ; a week
      projectile-file-exists-local-cache-expire (* 7 24 60 60) ; a week
      projectile-completion-system 'helm
      projectile-switch-project-action 'projectile-find-file-dwim
      projectile-switch-project 'helm-projectile)

;; replace projectile prefix key
(let ((map projectile-mode-map))
  (define-key map projectile-keymap-prefix nil)
  (define-key map (kbd "C-x C-p") 'projectile-command-map)
  (define-key map (kbd "C-x p") 'projectile-command-map))

(projectile-global-mode)
(helm-autoresize-mode 1)
(helm-projectile-on)


;; Backspace goes to the upper folder if you are not inside a filename,
;; and Return will select a file or navigate into the directory if it is one.
;; http://emacsist.com/10477
(defun my:helm-find-files-backward-dwim ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-backward-char 1)))

(define-key helm-read-file-map (kbd "<backspace>") 'my:helm-find-files-backward-dwim)
(define-key helm-read-file-map (kbd "DEL") 'my:helm-find-files-backward-dwim)
(define-key helm-find-files-map (kbd "<backspace>") 'my:helm-find-files-backward-dwim)
(define-key helm-find-files-map (kbd "DEL") 'my:helm-find-files-backward-dwim)


;; reconfigure helm autoresize max height by window configuration
(defvar my:helm-original-autoresize-max-height nil
  "The variable to keep default value of helm-autoresize-max-height")

(defconst my:helm-reconfigure-autoresize-ratio [ 1 0.7 0.5 0.3 ]
  "Resize ratio depends on vertical split")

(defun my:get-window-vertically-split-times (&optional win-tree)
  "Detect how many times windows are splitted vertically."
  (let ((tree (or win-tree (car (window-tree)))))
    (if (not (listp tree)) 0
      (let* ((vert (car tree))
             (childs (cddr tree))
             (len (length childs))
             (times 0))
        (dolist (c childs)
          (when (and c (listp c))
            (setq times (+ times (my:get-window-vertically-split-times c)))))
        (setq times (if vert (+ (1- len) times) times))
        times))))

(defun my:helm-reconfigure-autoresize-max ()
  "Reconfigure maximum height of helm on current window
configuration. If windows were laid vertically, max height
of helm would be shrinked."
  (setq my:helm-original-autoresize-max-height (or my:helm-original-autoresize-max-height
                                                   helm-autoresize-max-height))
  (let* ((vert (my:get-window-vertically-split-times))
         (ratio (condition-case nil
                    (aref my:helm-reconfigure-autoresize-ratio vert)
                  (error (aref my:helm-reconfigure-autoresize-ratio
                          (1- (length my:helm-reconfigure-autoresize-ratio))))))
         (calculated-max-height (truncate (* my:helm-original-autoresize-max-height ratio))))
    (setq helm-autoresize-max-height (max calculated-max-height helm-autoresize-min-height 5))))

(defun my:helm-reset-autoresize-max ()
  (setq helm-autoresize-max-height my:helm-original-autoresize-max-height))

(add-hook 'helm-before-initialize-hook 'my:helm-reconfigure-autoresize-max)
(add-hook 'helm-cleanup-hook 'my:helm-reset-autoresize-max)

;; http://emacs.stackexchange.com/questions/2563/helm-search-within-buffer-feature
(defconst my:helm-follow-sources
  '(helm-source-occur
    helm-ag-source
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
