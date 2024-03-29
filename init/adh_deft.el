;;; adh_deft.el --- Customizations for deft mode. -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 26 May 2015 19:55
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Customisations for Deft (http://jblevins.org/projects/deft/)

;;; Code:
(autoload 'deft "deft")

(setq deft-extensions '("org" "md"))
(setq deft-directory "/Users/adh/Documents/notes/")
(setq deft-recursive t)
; (setq deft-text-mode 'markdown-mode)

;; use file name rather than first line as note title
(setq deft-use-filename-as-title t)

;; set keybinding for deft
(global-set-key (kbd "C-c m d") 'deft)

(provide 'adh_deft)
;;; adh_deft.el ends here
