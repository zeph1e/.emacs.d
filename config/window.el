(use-package windswap
  :init
  (windmove-default-keybindings 'control)
  (windswap-default-keybindings 'shift))

(use-package windcopy
  :ensure nil
  :commands (windcopy-default-keybindings)
  :init
  (windcopy-default-keybindings 'control 'shift))

(use-package framemove
  :ensure nil
  :commands (framemove-default-keybindings)
  :init
  (framemove-default-keybindings 'meta)
  (setq framemove-hook-into-windmove t))

(use-package windsplit
  :ensure nil
  :bind
  (("C-x \\" . split-window-horizontally)
   ("C-x -"  . split-window-vertically)
   ("C-x |"  . windsplit-horizontally-and-move-right)
   ("C-x _"  . windsplit-vertically-and-move-down)
   ("C-x x"  . delete-window)
   ("C-{"    . shrink-window-horizontally)
   ("C-}"    . enlarge-window-horizontally))
  :commands
  (windsplit-horizontally-and-move-right
   windsplit-vertically-and-move-down))
