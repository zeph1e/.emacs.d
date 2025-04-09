;;; use-package-ensure-system-package+.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.

;; This modifies the behavior of system package installation from
;; `use-package-ensure-system-package'. It is originally not quite good
;; enough in the running Emacs at the first time on new environment.

(require 'use-package)
(require 'use-package-ensure-system-package)

(defvar upesp+:command-queue nil
  "The command queue to store requested command. It will be executed
sequentially in the order of first to last.")

(defvar upesp+:command-done nil
  "The alist which has pair of package manager and package name.
This is to prevent redundant execution.")

(defvar upesp+:command-occupied nil
  "It is to mark an install process is running.")

(defconst upesp+:package-manager-deps
  `(("apt" . nil)
    ("npm" . ("sudo apt install -y npm" "npm config set prefix ~/.local"))
    ("pip" . ("sudo apt install -y pip3")))
  "The list of commands to set up package managers dependencies")


(defun upesp+:command-need-execute (command)
  ;; (message "need execute: %S" command)
  (cond
   ((stringp command)
    (upesp+:command-need-execute (split-string-shell-command command)))
   ((listp command)
    (if (and (stringp (car command)) (string= (car command) "sudo"))
        (upesp+:command-need-execute (cdr command))
      (let* ((pair (cons (car command) (car (last command))))
             (found (and pair (car pair) (cdr pair)
                         (car (member pair upesp+:command-done)))))
        (unless found
          (push pair upesp+:command-done)
          pair))))
   (t (message "Unexpected %S" command) nil)))

(defun upesp+:get-package-manager-deps (package-manager)
  (unless (and (stringp package-manager)
               (executable-find package-manager))
    (cdr (assoc package-manager upesp+:package-manager-deps))))

(defun upesp+:async-shell-command (command &optional _out _err)
  ;; (message "executing: %S!!!!!!!!!!!!!!!!!" command)
  (when command
    (add-to-list 'upesp+:command-queue command))
  (unless upesp+:command-occupied
    (setq upesp+:command-occupied t)
    (let* ((cmd (pop upesp+:command-queue))
           (pkg (and cmd
                     (upesp+:command-need-execute cmd)))
           (deps (upesp+:get-package-manager-deps (car pkg))))
      ;; (message "cmd: %S pkg: %S deps: %s" cmd pkg deps)
      (if (and cmd pkg)
          (progn
            (when deps
              ;; there's some dependencies to be resolved
              (push cmd upesp+:command-queue)  ; push current command again
              (setq cmd (car deps))
              (setq upesp+:command-queue (append (cdr deps)
                                                 upesp+:command-queue)))
            (set-process-sentinel
             (let ((buffer (get-buffer-create
                            (format "*Installing Package[%s]*" (cdr pkg)))))
               (save-window-excursion
                 (display-buffer buffer '(display-buffer-pop-up-window))
                 (async-shell-command cmd buffer)
                 (get-buffer-process buffer)))
             (lambda (process _string)
               (setq upesp+:command-occupied nil)
               (kill-buffer (process-buffer process))
               (run-with-timer 0 nil #'upesp+:async-shell-command nil))))
        (setq upesp+:command-occupied nil)))))

;;;###autoload
(defun upesp+:use-package-ensure-system-package-consify (arg)
  "Turn ARG into a cons of the form (PACKAGE-NAME . INSTALL-COMMAND').
Replaced async-shell-command to upesp+:async-shell-command
to prevent to run package manager at the same time."
  (cond
   ((stringp arg)
    (cons arg `(system-packages-install ,arg)))
   ((symbolp arg)
    (cons arg `(system-packages-install ,(symbol-name arg))))
   ((consp arg)
    (cond
     ((not (cdr arg))
      (use-package-ensure-system-package-consify (car arg)))
     ((stringp (cdr arg))
      (progn
        (push (cdr arg) use-package-ensure-system-package--custom-packages)
        (cons (car arg) `(upesp+:async-shell-command ,(cdr arg)))))
     (t
      (cons (car arg)
            `(system-packages-install ,(symbol-name (cdr arg)))))))))

;;;###autoload
(advice-add 'use-package-ensure-system-package-consify
            :override #'upesp+:use-package-ensure-system-package-consify)

(provide 'use-package-ensure-system-package+)
