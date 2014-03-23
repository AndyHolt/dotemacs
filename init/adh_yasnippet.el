;;; adh_yasnippet.el --- Powerful snippet expansion

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:27
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Set up for Yasnippet mode.

;;; Code:

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")

(require 'yasnippet)

(yas-global-mode 1)
; taken directly from customize file - need to make work...
; can't get this to work - for now being done from custom.el file, written by customize
;(yas-global-mode t nil (yasnippet))

(set yas-indent-line 'auto)

;; if region is selected, wrap snippet around the region.
(setq yas-wrap-around-region t)

;; allow nested/stacked expansion (expansion within an expansion)
(setq yas-triggers-in-field t)

;; reactivate snippet fields on undo/redo
(setq yas-snippet-revival t)

(provide 'adh_yasnippet)

;;; adh_yasnippet.el ends here
