;;; adh_dired.el --- Config for Dired -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 28 Mar 2020 19:26
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set variable values and add useful functions for dired.

;;; Code:

(require 'dired)

;; change C-< and C-> to jump to more logical places
(defun dired-back-to-top ()
  "Move to first no . or .. file in directory"
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  "Move to last file in directory"
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; set up peep-dired, for displaying images etc while browsing in dired
(require 'peep-dired)
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "P") 'peep-dired))

(setq peep-dired-cleanup-on-disable t)
(setq peep-dired-ignored-extensions '("mkv" "iso" "mp4"))

(provide 'adh_dired)
;;; adh_dired.el ends here
