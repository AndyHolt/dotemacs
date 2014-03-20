(require 'visual-regexp)

;; define keys for replace and query replace regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; for multiple cursors
(define-key global-map (kbd "C-c m r") 'vr/mc-mark)

(provide 'adh_visualregexp)
