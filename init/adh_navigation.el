;;; adh_navigation.el --- In buffer navigation setup

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:00
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up more powerful ways of navigating the current buffer.
;; Includes:
;;   - Ace jump mode
;;   - Goto line exention to show line numbers
;;   - Expand region and change-inner
;;   - Annoying arrows mode
;;   - Anzu mode

;;; Code:

;[todo] - replace ace with avy
;; setup ace-jump mode
(require 'ace-jump-mode)

;; setup keyboard shortcuts for ace-jump mode
(global-set-key "\C-cj" 'ace-jump-mode)
(global-set-key "\C-ck" 'ace-jump-mode-pop-mark)
(global-set-key "\C-cw" 'ace-jump-word-mode)

;; setup ace-link mode, for jumping to links in help/info windows
(ace-link-setup-default)

;; setup ace-window mode for fast selection of windows
(global-set-key (kbd "C-x p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; show line numbers only when using 'goto-line'
;;   from whattheemacsd - thanks Magnars!
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; expand region by semantic units
(require 'expand-region)
(global-set-key "\C-@" 'er/expand-region)

;; delete within semantic units (akin to expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; annoying arrows mode - annoy me if I'm navigating badly
;; (require 'annoying-arrows-mode)
;; (global-annoying-arrows-mode)

;; show number of search matches in mode line
(require 'anzu)
(global-anzu-mode t)

(provide 'adh_navigation)

;;; adh_navigation.el ends here
