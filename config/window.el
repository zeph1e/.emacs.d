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
