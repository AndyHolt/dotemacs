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

(provide 'adh_dired)
