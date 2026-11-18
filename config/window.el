;;; window.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

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

  ;; split horizontally first from :
  ;; http://www.emacswiki.org/emacs/HorizontalSplitting
  (defun my:split-window-prefer-horizonally (window)
    "If there's only one window (excluding any possibly active
     minibuffer), then split the current window horizontally."
    (if (and (one-window-p t)
             (not (active-minibuffer-window)))
        (let ((split-height-threshold nil))
          (split-window-sensibly window))
      (split-window-sensibly window)))
  (setq split-window-preferred-function 'my:split-window-prefer-horizonally)
  :bind
  (:map my:global-key-map
   ("C-M-q" . bury-buffer)
   ("C-S-M-q" . unbury-buffer)
   ("C-v" . my:scroll-up-command)
   ("M-v" . my:scroll-down-command)))

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
  :bind
  (:map my:global-key-map
   ("M-<left>"  . fm-left-frame)
   ("M-<right>" . fm-right-frame)
   ("M-<up>"    . fm-up-frame)
   ("M-<down>"  . fm-down-frame))
  :init
  (framemove-default-keybindings 'meta)
  (setq framemove-hook-into-windmove t)
  :config
  ;; Under WSLg, real keyboard focus is decided by Windows' own window
  ;; manager over the RDP channel, which ignores the X11-level
  ;; `_NET_ACTIVE_WINDOW' request that `select-frame-set-input-focus' (and
  ;; so framemove) sends. Emacs's own `selected-frame' still flips, which
  ;; makes it look like the switch worked, but keystrokes keep going to the
  ;; previously focused frame. Ask Windows directly instead.
  (defconst my:wsl-activate-window-script
    "Add-Type -TypeDefinition @'
using System;
using System.Text;
using System.Runtime.InteropServices;
public class My_WinActivate {
  public delegate bool EnumProc(IntPtr hWnd, IntPtr lParam);
  [DllImport(\"user32.dll\")] public static extern bool EnumWindows(EnumProc proc, IntPtr lParam);
  [DllImport(\"user32.dll\")] public static extern int GetWindowText(IntPtr hWnd, StringBuilder sb, int n);
  [DllImport(\"user32.dll\")] public static extern bool IsWindowVisible(IntPtr hWnd);
  [DllImport(\"user32.dll\")] public static extern bool SetForegroundWindow(IntPtr hWnd);
  [DllImport(\"user32.dll\")] public static extern bool BringWindowToTop(IntPtr hWnd);
}
'@
$target = \"%s\"
$cb = {
  param($h, $l)
  if ([My_WinActivate]::IsWindowVisible($h)) {
    $sb = New-Object System.Text.StringBuilder 256
    [void][My_WinActivate]::GetWindowText($h, $sb, 256)
    $t = $sb.ToString()
    if ($t -eq $target -or $t -like \"$target (*\") {
      [void][My_WinActivate]::BringWindowToTop($h)
      [void][My_WinActivate]::SetForegroundWindow($h)
    }
  }
  return $true
}
[void][My_WinActivate]::EnumWindows($cb, [IntPtr]::Zero)
"
    "PowerShell template that gives Win32 focus to the window titled, or
titled with a \" (DISTRO)\" suffix added by WSLg, the formatted-in target.")

  (defun my:wsl-escape-powershell-string (s)
    "Escape S for interpolation inside a double-quoted PowerShell string."
    (replace-regexp-in-string "[`$\"]" "`\\&" s))

  (defun my:wsl-activate-window (title)
    "Ask Windows to give real input focus to the WSLg window titled TITLE."
    (when (and my:wslp (executable-find "powershell.exe"))
      (condition-case nil
          (let ((proc (make-process
                       :name "wsl-activate-window" :buffer nil :noquery t
                       :connection-type 'pipe
                       :command (list "powershell.exe" "-NoProfile"
                                      "-NonInteractive" "-EncodedCommand"
                                      (base64-encode-string
                                       (encode-coding-string
                                        (format my:wsl-activate-window-script
                                                (my:wsl-escape-powershell-string
                                                 title))
                                        'utf-16le)
                                       t)))))
            (process-send-eof proc))
        (error nil))))

  (advice-add 'fm-next-frame :after
              (lambda (&rest _)
                (my:wsl-activate-window (frame-parameter (selected-frame)
                                                           'name)))))

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
