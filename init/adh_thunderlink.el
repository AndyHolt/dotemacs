;;; adh_thunderlink.el --- Link Emacs to Thunderbird email client -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:25
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Links to Thunderbird email client for org links.

;;; Code:

(org-add-link-type "thunderlink" 'org-thunderlink-open)

(defun org-thunderlink-open (link-path)
  "Opens an email in Thunderbird with ThunderLink."
  (interactive)
  (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" link-path)))

(provide 'adh_thunderlink)
;;; adh_thunderlink.el ends here
