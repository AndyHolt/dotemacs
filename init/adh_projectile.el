;;; adh_projectile.el --- Project navigation tool -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:07
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for projectile mode.

;;; Code:

;(require 'projectile)
;(projectile-mode)
(autoload 'projectile-command-map "projectile")

; (require 'helm-projectile)
(autoload 'projectile-command-map "helm-projectile")

; once projectile is called for loading, enable projectile-mode as this
; initialises some important variables required for use of projectile itself.
(eval-after-load 'projectile
  '(projectile-mode t))

(define-key global-map (kbd "C-c p") #'projectile-command-map)

(setq projectile-enable-caching t)

(setq projectile-ignored-projects '("~/Dropbox/"))

(setq projectile-mode-line-prefix " Proj")

(provide 'adh_projectile)
;;; adh_projectile.el ends here
