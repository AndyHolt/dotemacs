;;; adh_keyfreq.el --- log which key combinations are most used.

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:50
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for logging key commands

;;; Code:

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(provide 'adh_keyfreq)

;;; adh_keyfreq.el ends here
