;; things to do when starting emacs

(setq-default
  fill-column        80
  indent-tabs-mode   nil)

(global-font-lock-mode   t)
(global-auto-revert-mode t)
(auto-compression-mode   t)
(auto-image-file-mode    t)

(setq
 size-indication-mode  t
 require-final-newline t
 visible-cursor        nil)

(find-file (concat adh-dropbox-location "Org_files/todo.org"))

(provide 'adh_startup)
