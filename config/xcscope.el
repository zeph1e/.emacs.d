;;; xcscope.el  -*- lexical-binding: t; -*-

;; Written by Yunsik Jang <z3ph1e@gmail.com>
;; You can use/modify/redistribute this freely.


(use-package xcscope
  :ensure-system-package (gtags-cscope . "sudo apt install -y global")
  :init
  (cscope-setup)
  :config
  ;; gtags handles file discovery itself; skip cscope.files creation
  (advice-add 'cscope-make-index-command :around
              (lambda (orig dir only-create-list-of-files)
                (if (string= cscope-program "gtags-cscope")
                    (unless only-create-list-of-files
                      (if (file-exists-p
                           (concat (cscope-canonicalize-directory dir)
                                   cscope-database-file))
                          "global -u -v\n"
                        "gtags -v\n"))
                  (funcall orig dir only-create-list-of-files))))
  :custom
  (cscope-program "gtags-cscope")
  (cscope-database-file "GTAGS"))
