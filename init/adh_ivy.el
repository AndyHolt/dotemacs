;;; adh_ivy.el --- Setup for ivy selection -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Wed 26 Apr 2017 13:03
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Replacement for ido.

;;; Code:

(require 'ivy)
(ivy-mode 1)

;; set projectile to use ivy
(setq projectile-completion-system 'ivy)

;; set mu4e to use ivy
(setq mu4e-completing-read-function 'ivy-completing-read)

(defvar ivy-display-functions-alist
  '((ivy-completion-in-region . ivy-display-function-overlay))
  "An alist for customizing `ivy-display-function'.")

;; use counsel-M-x as a ivy version of M-x execute-exended-command/smex
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; turn on counsel-mode, to use ivy/counsel versions of many commands
(counsel-mode 1)

;; set up ivy for fuzzy matching
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
;; and when fuzzy matching, don't want default input to start with "^" to match
;; from beginning of line
(setq ivy-initial-inputs-alist nil)

;; Enable selecting prompt: use current input as candidate
(setq ivy-use-selectable-prompt t)

(provide 'adh_ivy)
;;; adh_ivy.el ends here
