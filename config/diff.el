;;; diff.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

(use-package diff-mode
  :ensure nil
  :pin manual
  :custom-face
  (diff-added ((((class color) (min-colors 88) (background dark))
                (:foreground nil :background "#335533"))
               (((class color) (min-colors 88) (background light))
                (:foreground nil :background "green"))
               (t (:foreground nil :background "#335533"))))
  (diff-removed ((((class color) (min-colors 88) (background dark))
                  (:foreground nil :background "#553333"))
                 (((class color) (min-colors 88) (background light))
                  (:foreground nil :background "#FF8888"))
                 (t (:foreground nil :background "#553333"))))
  (diff-refine-added ((((class color) (min-colors 88) (background dark))
                       (:foreground "white" :background "darkgreen"))
                      (((class color) (min-colors 88) (background light))
                       (:foreground "black" :background "lawn green"))
                      (t (:foreground "white" :background "darkgreen"))))
  (diff-refine-removed ((((class color) (min-colors 88) (background dark))
                         (:foreground "white" :background "darkred"))
                        (((class color) (min-colors 88) (background light))
                         (:foreground "black" :background "#FFBBBB"))
                         (t (:foreground "white" :background "darkred"))))
  (diff-refine-changed ((((class color) (min-colors 88) (background dark))
                         (:foreground "white" :background "darkblue"))
                        (((class color) (min-colors 88) (background light))
                         (:foreground "black" :background "cyan"))
                        (t (:foreground "white" :background "darkblue"))))
  :hook
  (diff-mode . (lambda ()
                 (setq-local whitespace-style
                             '(face trailing tabs tab-mark))
                 (whitespace-mode 1))))

(use-package ediff
  :ensure nil
  :pin manual
  :init
  ;; ediff help functions
  (defun my:ediff-copy-both-to-C ()
    "Copy the current diff's A and B regions concatenated into buffer C."
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
    (add-hook 'ediff-keymap-setup-hook
              (lambda ()
                (define-key ediff-mode-map "c" 'my:ediff-copy-both-to-C)))
  :custom
  ((ediff-window-setup-function 'ediff-setup-windows-plain)
   (ediff-split-window-function 'split-window-horizontally)))
