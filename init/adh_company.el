;;; adh_company.el --- Set up company (completions) -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Wed 14 May 2025 12:07
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Configure company mode completions/suggestions.

;;; Code:

(require 'company-autoloads)


;; TODO this should probably be evaled after company loads instead?
(require 'company)
(setq company-backends '((company-capf company-dabbrev-code)))

(add-hook 'prog-mode-hook 'company-mode)

(provide 'adh_company)
;;; adh_company.el ends here
