;;; utils-planner.el

;; Written by Yunsik Jang <doomsday@kldp.org>
;; You can use/modify/redistribute this freely

(setq planner-project "MyPlanner")
(setq muse-project-alist
      '(("MyPlanner"
         ("~/plans"
          :default "index"
          :major-mode planner-mode
          :visit-link planner-visit-link))))
(or (file-exists-p "~/plans") (make-directory "~/plans"))

(provide 'utils-planner)
