(require 'multiple-cursors)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; some keybindings
(define-key global-map (kbd "C-c m l") 'mc/edit-lines)

(provide 'adh_multiplecursors)
