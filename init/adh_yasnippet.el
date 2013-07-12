;; setup yasnippet
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")

(require 'yasnippet)

(yas/global-mode 1)
; taken directly from customize file - need to make work...
; can't get this to work - for now being done from custom.el file, written by customize
;(yas-global-mode t nil (yasnippet))

(set yas-indent-line 'auto)

;; if region is selected, wrap snippet around the region.
(setq yas-wrap-around-region t)

(provide 'adh_yasnippet)
