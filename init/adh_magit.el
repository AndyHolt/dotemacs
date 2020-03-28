;;; adh_magit.el --- set up for Magit -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 29 Dec 2014 18:00
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for Magit - an Emacs mode for git.

;;; Code:

(require 'magit)
(global-set-key (kbd "C-c m s") 'magit-status)

(provide 'adh_magit)

;;; adh_magit.el ends here
