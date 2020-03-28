;;; adh_smartmx.el --- Smart M-x mode -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:11
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Better M-x mode.
;; Suggests previous commands and easy searching.

;;; Code:

(require 'smex)
(smex-initialize)

;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; old M-x
; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'adh_smartmx)

;;; adh_smartmx.el ends here
