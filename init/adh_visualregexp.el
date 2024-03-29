;;; adh_visualregexp.el --- Help build regexps -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:27
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Helps with building regexps by highlighting matches in the buffer.

;;; Code:

(autoload 'vr/replace "visual-regexp" "" t)
(autoload 'vr/query-replace "visual-regexp" "" t)

;; define keys for replace and query replace regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; for multiple cursors
(autoload 'vr/mc-mark "visual-regexp" "" t)
(define-key global-map (kbd "C-c m r") 'vr/mc-mark)

(provide 'adh_visualregexp)
;;; adh_visualregexp.el ends here
