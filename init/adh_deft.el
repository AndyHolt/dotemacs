;;; adh_deft.el --- Customizations for deft mode.

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 26 May 2015 19:55

;;; Commentary:
;;
;; Customisations for Deft (http://jblevins.org/projects/deft/)

;;; Code:

(setq deft-extension "md")
(setq deft-directory "~/Dropbox/Notes")
(setq deft-text-mode 'markdown-mode)

;; use file name rather than first line as note title
(setq deft-use-filename-as-title t)

;; set keybinding for deft
(global-set-key (kbd "C-c m d") 'deft)

(provide 'adh_deft)

;;; adh_deft.el ends here
