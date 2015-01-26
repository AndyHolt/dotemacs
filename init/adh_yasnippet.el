;;; adh_yasnippet.el --- Powerful snippet expansion

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:27
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Set up for Yasnippet mode.

;;; Code:

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")

(require 'yasnippet)

;; set directories for snippets to be found
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)
; taken directly from customize file - need to make work...
; can't get this to work - for now being done from custom.el file, written by customize
;(yas-global-mode t nil (yasnippet))

(set yas-indent-line 'auto)

;; if region is selected, wrap snippet around the region.
(setq yas-wrap-around-region t)

;; allow nested/stacked expansion (expansion within an expansion)
(setq yas-triggers-in-field t)

;; reactivate snippet fields on undo/redo
(setq yas-snippet-revival t)

;; use helm to select from multiple possible snippets.
(defun adh-yas-helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas-prompt-functions'."
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x))
                           choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdf (assoc result rmap))))
    nil))

(setq yas-prompt-functions '(shk-yas/helm-prompt yas-ido-prompt yas-no-prompt))


(provide 'adh_yasnippet)

;;; adh_yasnippet.el ends here
