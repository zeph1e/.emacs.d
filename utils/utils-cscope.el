;;; utils-cscope.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

;; Redefine cscope-search-one-database to use dir-locals variables
(eval-after-load 'xcscope
  `(defun cscope-search-one-database ()
     "Pop a database entry from `cscope-search-list' and do a search there."

     (let ( next-item options cscope-directory database-file outbuf done
                      base-database-file-name)
       (setq outbuf (get-buffer-create cscope-output-buffer-name))
       (save-excursion
         (catch 'finished
           (set-buffer outbuf)
           (setq options '("-L"))
           (while (and (not done) cscope-search-list)
             (setq next-item (car cscope-search-list)
                   cscope-search-list (cdr cscope-search-list)
                   base-database-file-name cscope-database-file
                   )
             (if (listp next-item)
                 (progn
                   (setq cscope-directory (car next-item))
                   (if (not (stringp cscope-directory))
                       (setq cscope-directory
                             (cscope-search-directory-hierarchy
                              default-directory)))
                   (if (file-regular-p cscope-directory)
                       (progn
                         ;; Handle the case where `cscope-directory' is really
                         ;; a full path name to a cscope database.
                         (setq base-database-file-name
                               (file-name-nondirectory cscope-directory)
                               cscope-directory
                               (file-name-directory cscope-directory))
                         ))
                   (setq cscope-directory 
                         (file-name-as-directory cscope-directory))
                   (when (not (member cscope-directory cscope-searched-dirs))
                     (push cscope-directory cscope-searched-dirs)
                     (setq done t))
                   )
               (progn
                 (if (and cscope-first-match-point
                          cscope-stop-at-first-match-dir
                          cscope-stop-at-first-match-dir-meta)
                     (throw 'finished nil))
                 ))
             )
           (if (not done)
               (throw 'finished nil))

           (when (cadr next-item)
             (let ((newopts (cadr next-item)))
               (unless (listp newopts)
                 (error (format "Cscope options must be a list: %s" newopts)))
               (setq options (append options newopts))))
           (if cscope-command-args
               (setq options (append options cscope-command-args))
             (error "Tried to do a search without 'cscope-command-args' set. Something is wrong..."))


           (setq database-file (concat cscope-directory base-database-file-name)
                 cscope-searched-dirs (cons cscope-directory
                                            cscope-searched-dirs)
                 )

           (setq default-directory cscope-directory)
           (hack-dir-local-variables-non-file-buffer)

           ;; The database file and the directory containing the database file
           ;; must both be writable.
           (if (or (not (file-writable-p database-file))
                   (not (file-writable-p (file-name-directory database-file)))
                   cscope-option-do-not-update-database)
               (setq options (cons "-d" options)))


           ;; is this require for multiple databases?
           ;; (goto-char (point-max))
           (if (string= base-database-file-name cscope-database-file)
               (insert (concat "\n" cscope-database-directory-prompt ": ")
                       (cscope-boldify-if-needed cscope-directory)
                       "\n\n")
             (insert (concat "\n" cscope-database-directory-prompt "/file: ")
                     (cscope-boldify-if-needed cscope-directory base-database-file-name)
                     "\n\n"))
           ;; Add the correct database file to search
           (setq options (cons base-database-file-name options))
           (setq options (cons "-f" options))
           (setq cscope-output-start (point))

           (setq cscope-process-output nil
                 cscope-last-file nil
                 )
           (setq cscope-process
                 ;; Communicate with a pipe. Slightly more efficient than
                 ;; a TTY
                 (let ((process-connection-type nil))
                   (apply cscope-start-file-process "cscope" outbuf
                          cscope-program
                          (append (cscope-construct-custom-options-list) options))))
           (set-process-filter cscope-process 'cscope-process-filter)
           (set-process-sentinel cscope-process 'cscope-process-sentinel)
           (setq cscope-last-output-point (point))
           (process-kill-without-query cscope-process)
           (if cscope-running-in-xemacs
               (setq modeline-process ": Searching ..."))
           t
           ))
       ))
)

(provide 'utils-cscope)
