;; setup ace-jump mode
(require 'ace-jump-mode)

;; setup keyboard shortcuts for ace-jump mode
(global-set-key "\C-cj" 'ace-jump-mode)
(global-set-key "\C-ck" 'ace-jump-mode-pop-mark)
(global-set-key "\C-cw" 'ace-jump-word-mode)

;; show line numbers only when using 'goto-line'
;;   from whattheemacsd - thanks Magnars!
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
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

;; annoying arrows mode - annoy me if I'm navigating badly
(require 'annoying-arrows-mode)
(global-annoying-arrows-mode)

(provide 'adh_navigation)
