;;; adh_flycheck.el --- Set up flycheck -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 28 Mar 2020 19:35
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Configuration of flycheck syntax checking

;;; Code:

(autoload 'flycheck-mode "flycheck")

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

; (global-flycheck-mode)

;; (eval-after-load 'python-mode
;;   '(flycheck-mode))
;; (eval-after-load 'tex-mode
;;   '(flycheck-mode))
;; (eval-after-load 'elisp-mode
;;   '(flycheck-mode))

(defun adh-enable-flycheck-mode (&rest args)
    "Enable flycheck mode.
Function intended for use in major mode hooks. Ignores args."
    (flycheck-mode t))

; (add-hook 'mode-hook #'my-function)
(add-hook 'python-mode-hook #'adh-enable-flycheck-mode)
(add-hook 'TeX-mode-hook #'adh-enable-flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'adh-enable-flycheck-mode)

(provide 'adh_flycheck)
;;; adh_flycheck.el ends here
