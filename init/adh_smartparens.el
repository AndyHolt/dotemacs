;;; adh_smartparens.el --- Smart parentheses -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:15
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Smarter parentheses - inserts pairs of various punctuation marks.
;; (", ', (, {, [ etc)

;;; Code:

(require 'smartparens)
; use smartparens mode instead of auto-pair and show-paren-mode
(smartparens-global-mode t)
(require 'smartparens-config)
(show-smartparens-global-mode t)

;; (sp-with-modes '(tex-mode
;;                  plain-tex-mode
;;                  latex-mode)
;;   (sp-local-pair "\\$" nil :actions nil)
;;   (sp-local-pair "$" "$"))

(provide 'adh_smartparens)
;;; adh_smartparens.el ends here
