;;; adh_fullframe.el --- Settings for fullframe

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 24 Jan 2015 15:08
;; URL: https://github.com/AndyHolt/dotemacs/init/adh_fullframe.el

;;; Commentary:
;;
;; Settings for fullframe package.
;;
;; Allows opening of certain buffers/applications in full screen mode, going
;; back to previous window configurations when finished.

;;; Code:

(require 'fullframe)

;; set magit to use fullframe
(fullframe magit-status magit-mode-quit-window)

(provide 'adh_fullframe)

;;; adh_fullframe.el ends here
