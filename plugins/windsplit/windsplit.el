;;; windsplit.el

(require 'windmove)

;;;###autoload
(defun windsplit-horizontally-and-move-right (&optional size)
  "Split the selected window horizontall and move focus to right"
  (interactive "P")
  (split-window-horizontally size)
  (windmove-right))

;;;###autoload
(defun windsplit-vertically-and-move-down (&optional size)
"Split the selected window vertically and move focus to down"
  (interactive "P")
  (split-window-vertically size)
  (windmove-down))

(provide 'windsplit)
