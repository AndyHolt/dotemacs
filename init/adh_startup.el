;; things to do when starting emacs

(defvar adh-dropbox-location
  (cond ((eq system-type 'gnu-linux) "~/Dropbox/")
	((eq system-type 'windows-nt) "d:/Dropbox/"))
  "Holds the location of the Dropbox root directory for the current system")

(find-file (concat adh-dropbox-location "Org_files/todo.org"))

(provide 'adh_startup)
