;;; markdown.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package markdown-mode
  :ensure-system-package
  (markdown . "sudo apt install -y markdown")
  (pandoc . "sudo apt install -y pandoc")
  :config
  ;; Use pandoc for markdown preview
  (let* ((template-submodule "pandoc/pandoc-toc-sidebar")
         (template-dir (expand-file-name (concat user-emacs-directory
                                                 template-submodule))))
    ;; populate pandoc template submodule if it is not
    (unless (and (file-exists-p template-dir)
                (not (directory-empty-p template-dir)))
      (with-temp-buffer
        (let ((default-directory user-emacs-directory))
          (magit-submodule-populate "pandoc/pandoc-toc-sidebar" nil))))

    ;; set convert command with pandoc
    (let ((template (concat template-dir "/toc-sidebarL.html"))
          (nav (concat template-dir "/nav")))
    (setq markdown-command
          (format (concat "pandoc -f markdown -t html --template=%s --toc "
                          "-B %s --resource-path=%s "
                          "--embed-resources --standalone")
                  template nav template-dir))))

  ;; Beautifying tip fetched from:
  ;; https://www.reddit.com/r/emacs/comments/10h9jf0/beautify_markdown_on_emacs/
  (defvar nb/current-line '(0 . 0)
    "(start . end) of current line in current buffer")
  (make-variable-buffer-local 'nb/current-line)

  (defun nb/unhide-current-line (limit)
    "Font-lock function"
    (let ((start (max (point) (car nb/current-line)))
          (end (min limit (cdr nb/current-line))))
      (when (< start end)
        (remove-text-properties start end
                                '(invisible t display "" composition ""))
        (goto-char limit))
      ;; Return nil: this is a side-effecting matcher with no highlight
      ;; subexpressions, so it must not claim a match (a non-nil return makes
      ;; font-lock drive its match-data machinery with positions we never set,
      ;; yielding "wrong-type-argument number-or-marker-p nil").
      nil))

  (defun nb/refontify-on-linemove ()
    "Post-command-hook"
    (let* ((start (line-beginning-position))
           (end (line-beginning-position 2))
           (needs-update (not (equal start (car nb/current-line)))))
      (setq nb/current-line (cons start end))
      (when needs-update
        (font-lock-fontify-block 3))))

  (defun nb/markdown-unhighlight ()
    "Enable markdown concealling"
    (interactive)
    (markdown-toggle-markup-hiding 'toggle)
    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

  :custom-face
  (markdown-header-delimiter-face
   ((((background dark))  (:foreground "#616161" :height 0.9))
    (((background light)) (:foreground "#a0a0a0" :height 0.9))))
  ;; H1: Nord Green (Dark) vs Optimistic Coral Pink (Light)
  (markdown-header-face-1
   ((((background dark))  (:height 1.6 :foreground "#A3BE8C"
                           :weight extra-bold :inherit markdown-header-face))
    (((background light)) (:height 1.6 :foreground "#ff6b6b"
                           :weight extra-bold :inherit markdown-header-face))))
  ;; H2: Nord Yellow (Dark) vs Optimistic Sunny Orange (Light)
  (markdown-header-face-2
   ((((background dark))  (:height 1.4 :foreground "#EBCB8B"
                           :weight extra-bold :inherit markdown-header-face))
    (((background light)) (:height 1.4 :foreground "#f39c12"
                           :weight extra-bold :inherit markdown-header-face))))
  ;; H3: Nord Orange (Dark) vs Optimistic Warm Amber (Light)
  (markdown-header-face-3
   ((((background dark))  (:height 1.2 :foreground "#D08770"
                           :weight extra-bold :inherit markdown-header-face))
    (((background light)) (:height 1.2 :foreground "#d35400"
                           :weight extra-bold :inherit markdown-header-face))))
  ;; H4: Nord Red (Dark) vs Optimistic Mint/Teal (Light)
  (markdown-header-face-4
   ((((background dark))  (:height 1.15 :foreground "#BF616A"
                           :weight bold :inherit markdown-header-face))
    (((background light)) (:height 1.15 :foreground "#1abc9c"
                           :weight bold :inherit markdown-header-face))))
  ;; H5: Nord Purple (Dark) vs Optimistic Sky Blue (Light)
  (markdown-header-face-5
   ((((background dark))  (:height 1.1 :foreground "#b48ead"
                           :weight bold :inherit markdown-header-face))
    (((background light)) (:height 1.1 :foreground "#3498db"
                           :weight bold :inherit markdown-header-face))))
  ;; H6: Nord Blue (Dark) vs Optimistic Deep Amethyst (Light)
  (markdown-header-face-6
   ((((background dark))  (:height 1.05 :foreground "#5e81ac"
                           :weight semi-bold :inherit markdown-header-face))
    (((background light)) (:height 1.05 :foreground "#9b59b6"
                           :weight semi-bold :inherit markdown-header-face))))
  :hook
  (markdown-mode . nb/markdown-unhighlight)
  :custom
  ((markdown-split-window-direction 'right)
   (markdown-coding-system 'utf-8)))
