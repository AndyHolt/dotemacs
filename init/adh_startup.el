;; things to do when starting emacs

(setq-default
  fill-column        80
  indent-tabs-mode   nil)

(global-font-lock-mode   t)

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(auto-compression-mode   t)
(auto-image-file-mode    t)

(setq
 size-indication-mode  t
 require-final-newline t
 visible-cursor        nil)

(setq-default indicate-empty-lines t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; easily navigate camelCase etc words
(global-subword-mode 1)

(find-file (concat adh-dropbox-location "Org_files/todo.org"))

(provide 'adh_startup)
