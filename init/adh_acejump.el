(require 'ace-jump-mode)

;;; setup keyboard shortcuts for ace-jump mode

(global-set-key "\C-cj" 'ace-jump-mode)
(global-set-key "\C-ck" 'ace-jump-mode-pop-mark)
(global-set-key "\C-cw" 'ace-jump-word-mode)

(provide 'adh_acejump)
