;; saving between one session and another

;; keep session files in .cache directory:

(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

(provide 'adh_sessions)
