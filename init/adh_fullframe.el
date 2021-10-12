;;; adh_fullframe.el --- Settings for fullframe -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 24 Jan 2015 15:08
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Settings for fullframe package.
;;
;; Allows opening of certain buffers/applications in full screen mode, going
;; back to previous window configurations when finished.

;;; Code:

(autoload 'fullframe "fullframe")

;; set magit to use fullframe
(eval-after-load "magit"
  '(fullframe magit-status magit-mode-quit-window))

(provide 'adh_fullframe)
;;; adh_fullframe.el ends here
