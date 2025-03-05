(use-package windswap
  :bind
  (:map my:global-key-map
   ("C-<left>"  . windmove-left)
   ("C-<right>" . windmove-right)
   ("C-<up>"    . windmove-up)
   ("C-<down>"  . windmove-down)
   ("S-<left>"  . windswap-left)
   ("S-<right>" . windswap-right)
   ("S-<up>"    . windswap-up)
   ("S-<down>"  . windswap-down))
  :init
  (windmove-default-keybindings 'control)
  (windswap-default-keybindings 'shift))

(use-package windcopy
  :ensure nil
  :commands (windcopy-default-keybindings)
  :bind
  (:map my:global-key-map
   ("C-S-<left>"  . windcopy-left)
   ("C-S-<right>" . windcopy-right)
   ("C-S-<up>"    . windcopy-up)
   ("C-S-<down>"  . windcopy-down))
  :init
  (windcopy-default-keybindings 'control 'shift))

(use-package framemove
  :ensure nil
  :commands (framemove-default-keybindings)
  :bind
  (:map my:global-key-map
   ("M-<left>"  . fm-left-frame)
   ("M-<right>" . fm-right-frame)
   ("M-<up>"    . fm-left-frame)
   ("M-<down>"  . fm-down-frame))
  :init
  (framemove-default-keybindings 'meta)
  (setq framemove-hook-into-windmove t))

(use-package windsplit
  :ensure nil
  :bind
  (:map my:global-key-map
   ("C-x \\" . split-window-horizontally)
   ("C-x -"  . split-window-vertically)
   ("C-x |"  . windsplit-horizontally-and-move-right)
   ("C-x _"  . windsplit-vertically-and-move-down)
   ("C-x x"  . delete-window)
   ("C-{"    . shrink-window-horizontally)
   ("C-}"    . enlarge-window-horizontally))
  :commands
  (windsplit-horizontally-and-move-right
   windsplit-vertically-and-move-down))

(use-package window
  :ensure nil
  :pin manual
  :config
  (defun my:scroll-up-command (&optional arg)
    "Modify scroll-up behavior to make it move to the end of buffer."
    (interactive "P")
    (if (eq (point)(point-max))
        (signal 'end-of-buffer '())
      (condition-case e
          (scroll-up-command arg)
        (end-of-buffer (goto-char (point-max))))))

  (defun my:scroll-down-command (&optional arg)
    "Modify scroll-down behaviour to make it move to the beginning of buffer."
    (interactive "P")
    (if (eq (point)(point-min))
        (signal 'beginning-of-buffer '())
      (condition-case e
          (scroll-down-command arg)
        (beginning-of-buffer (goto-char (point-min))))))

  :bind
  (:map my:global-key-map
   ("C-M-q" . bury-buffer)
   ("C-S-M-q" . unbury-buffer)
   ("C-v" . my:scroll-up-command)
   ("M-v" . my:scroll-down-command)))
