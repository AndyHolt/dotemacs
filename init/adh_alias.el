;;; adh_alias.el --- Function aliases -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:31
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Shortened forms of commonly used functions.
;; Call using M-x alias

;;; Code:

(defalias 'rs 'replace-string)
(defalias 'cc 'calc)
(defalias 'eb 'eval-buffer)
(defalias 'lf 'load-file)
;; always use ibuffer
(defalias 'list-buffers 'ibuffer)
(defalias 'ms 'magit-status)
(defalias 'qrr 'query-replace-regexp)

(provide 'adh_alias)

;;; adh_alias.el ends here
