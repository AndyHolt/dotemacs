;;; adh_hippie.el --- Set up hippie expand mode -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:46
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for more powerful text completion.

;;; Code:

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list '(yas/hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(provide 'adh_hippie)

;;; adh_hippie.el ends here
