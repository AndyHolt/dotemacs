(require 'flycheck)

;; (defun adh/adjust-flycheck-automatic-syntax-eagerness ()
;;   "Adjust how often we check for errors based on if there are any.

;; If there are errors, find as fast as possible, but in a clean buffer can be much
;; laxer"
;;   (setq flycheck-idle-change-delay
;;         (if flycheck-current-errors 0.5 5.0)))

;; ;; each buffer gets its own idle-change-delay because of the buffer-sensitive
;; ;; adjustment above.
;; (make-variable-buffer-local 'flycheck-idle-change-delay)

;; (add-hook 'flycheck-after-syntax-check-hook
;;           'adh/adjust-flycheck-automatic-syntax-eagerness)

;; ;; remove newline checks, since they would trigger an immediate check when we
;; ;; want the idle-change-delay to be in effect while editing.
;; (setq flycheck-check-syntax-automatically '(save
;;                                             idle-change
;;                                             mode-enabled))

;; (defun flycheck-handle-idle-change ()
;;   "Handle an expired idle time since the last change.

;; This is an overwritten version of the original flycheck-handle-idle-change,
;; which removes the forced deferred.
;; Timers should only trigger inbetween commands in a single threaded system and
;; the forced deferred makes errors never show up before you execute another
;; command."
;;   (flycheck-clear-idle-change-timer)
;;   (flycheck-buffer-automatically 'idle-change))

(global-flycheck-mode)

(provide 'adh_flycheck)
