;;; adh_multiplecursors.el --- Setup multiple cursors -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:59
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Use multiple cursors for powerful editing.

;;; Code:

(require 'multiple-cursors)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; some keybindings
(define-key global-map (kbd "C-c m l") 'mc/edit-lines)

;; when inserting numbers, start count at 1, not 0 by default.
(setq mc/insert-numbers-default 1)

(provide 'adh_multiplecursors)

;;; adh_multiplecursors.el ends here
