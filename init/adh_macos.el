;;; adh_macos.el --- MacOS specific config file -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 11 Aug 2015 15:29
;; URL: https://githib.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Settings, functions and keybindings for use in emacs on MacOS which aren't
;; needed or are different on other operating systems.

;;; Code:

;; Allow hash to be entered
(defun insert-hash ()
  "Insert a hash sign (#) at cursor location."
  (insert "#"))

(global-set-key (kbd "M-3") #'(lambda() (interactive) (insert-hash)))

;; set keys for Apple keyboard, for emacs on macOS
(setq mac-command-modifier 'super) ; make cmd key do Super
(setq mac-option-modifier 'meta) ; make opt key do Meta
(setq mac-control-modifier 'control) ; make Control key do Control
(setq mac-function-modifier 'hyper)  ; make Fn key do Hyper
(setq mac-right-command-modifier 'super) ; make right cmd do Super
(setq mac-right-option-modifier 'meta) ; make right opt do Meta

;; make fn key + 3 insert a gbp pound sign (£)
(defun insert-gbp ()
  "Insert a pound (sterling) sign (£) at cursor location."
  (insert "£"))

(global-set-key (kbd "H-3") #'(lambda() (interactive) (insert-gbp)))

(provide 'adh_macos)
;;; macos.el ends here
