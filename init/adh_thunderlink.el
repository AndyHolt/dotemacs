(org-add-link-type "thunderlink" 'org-thunderlink-open)

(defun org-thunderlink-open (path)
  "Opens an email in Thunderbird with ThunderLink."
  (interactive)
  (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path)))

(provide 'adh_thunderlink)
