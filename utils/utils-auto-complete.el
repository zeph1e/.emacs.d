;;; utils-auto-complete.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely.

(defconst my:achead-std-begin-exp "^#include .+ starts here:$")
(defconst my:achead-std-end-exp "^End of search list.$")
(defun my:achead-find-std-headers (lang)
  (let* ((proc (start-process "my:achead"
                              nil
                              (or (getenv "CC")
                                  (executable-find "gcc")
                                  (executable-find "cc")
                                  (error "No compiler found"))
                              (concat "-x" (downcase (or (and (stringp lang) lang)
                                                         (signal 'wrong-type-argument (list lang)))))
                              "-E" "-V" "-"))
         (buf (process-buffer proc)))
    (set-process-filter proc 'my:achead-find-std-headers-filter)
    (set-process-sentinel proc 'my:achead-find-std-headers-sentinel)
    (with-current-buffer buf
      (buffer-string))))

(defun my:achead-find-std-headers-filter (process output))
(defun my:achead-find-std-headers-sentinel (process event))


;; include subdirectories like gtk+-2.0 or QtCore for auto-complete-c-headers
(defconst my:achead-subdir-regexp "[A-Za-z0-9-_]+\\-[0-9.]+\\|Qt[A-Za-z]+")
(defvar my:achead-searched-subdir nil)
(unless my:achead-searched-subdir
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
    (setq achead:include-directories(append achead:include-directories includedirs)
          my:achead-searched-subdir t)))

