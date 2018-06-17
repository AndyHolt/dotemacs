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

(setq projectile-enable-caching t)

(provide 'adh_projectile)

;;; adh_projectile.el ends here
