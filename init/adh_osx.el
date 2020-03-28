;;; adh_osx.el --- OSX specific config file -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 11 Aug 2015 15:29
;; URL: https://githib.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Settings, functions and keybindings for use in emacs on OS X which aren't
;; needed or are different on other operating systems.

;;; Code:

;; Allow hash to be entered
(defun insert-hash ()
  "Insert a hash sign (#) at cursor location."
  (insert "#"))

(global-set-key (kbd "M-3") '(lambda() (interactive) (insert-hash)))

(provide 'adh_osx)

;;; osx.el ends here
