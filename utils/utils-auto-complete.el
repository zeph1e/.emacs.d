;;; utils-auto-complete.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defconst my:achead-std-begin-exp "^#include .+ starts here:$")
(defconst my:achead-std-end-exp "^End of search list.$")
(defvar my:achead-std-begin-exp-found nil)

;; grap some include paths from compiler
(defun my:achead-find-std-headers (lang)
  (interactive "sLang:")
  (let ((buffer-name "*my:achead-find-std*"))
  (when (= 0 (call-process (or (getenv "CC")
                               (executable-find "gcc")
                               (executable-find "cc")
                               (error "No compiler found"))
                           nil buffer-name nil
                           (concat "-x" (downcase (or (and (stringp lang) lang)
                                                      (signal 'wrong-type-argument (list lang)))))
                           "-E" "-v" "-"))
    (with-current-buffer (get-buffer buffer-name)
      (let ((lines (split-string (buffer-string) "\n"))
            result
            skip-line)
        (save-excursion
          (goto-char 0)
          (dolist (line lines)
            (setq skip-line nil)
            (message "line: %s" line)
            (if (string-match my:achead-std-begin-exp line)
                (progn (setq my:achead-std-begin-exp-found t)
                       (message "%S" "found!")
                       (setq skip-line t))
              (if (string-match my:achead-std-end-exp line)
                  (setq skip-line t))
              (if (and (not skip-line) my:achead-std-begin-exp-found)
                  (dolist (w (split-string line))
                    (if (and (> (length w) 0) (string-prefix-p "/" w))
                        (push w result)))))))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))
        result)))))

;; include subdirectories like gtk+-2.0 or QtCore for auto-complete-c-headers
(defconst my:achead-subdir-regexp "[A-Za-z0-9-_]+\\-[0-9.]+\\|Qt[A-Za-z]+")
(defun my:achead-find-subdirs ()
  (let* ((cc (or (getenv "CC") (executable-find "gcc") (executable-find "cc")))
         (arch (and cc (substring (shell-command-to-string (concat cc " -dumpmachine")) 0 -1)))
         (readd (and (executable-find "uname")
                     (string-prefix-p (substring (shell-command-to-string "uname -m") 0 -1) arch)
                     (concat "\\|" arch)))
         includedirs)
    (dolist (d achead:include-directories)
      (and (file-directory-p d)
           (string-prefix-p "/" d)
           (dolist (subd (directory-files d nil (concat my:achead-subdir-regexp (or readd ""))))
             (and (file-directory-p (expand-file-name subd d))
                  (push (expand-file-name subd d) includedirs)))))
    includedirs))

(defvar my:achead-expanded nil)
(when (not my:achead-expanded)
  (dolist (path (append (my:achead-find-std-headers "c")
                        (my:achead-find-std-headers "c++")
                        (my:achead-find-subdirs)))
    (add-to-list 'achead:include-directories path)))
