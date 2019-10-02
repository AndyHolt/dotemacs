;;; adh_projectile.el --- Project navigation tool

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:07
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for projectile mode.

;;; Code:

(require 'projectile)
(projectile-global-mode)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-enable-caching t)

(setq projectile-ignored-projects '("~/Dropbox/"))

(provide 'adh_projectile)

;;; adh_projectile.el ends here
