;;; adh_sessions.el --- Save things between sessions -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:10
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Save sessions to allow restoring a previous one.

;;; Code:

;; keep session files in .cache directory:
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

(provide 'adh_sessions)

;;; adh_sessions.el ends here
