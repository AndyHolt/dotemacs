;;; adh_diminish.el --- Clean up modeline clutter

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:39
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Clean up modeline by shortening mode names.

;;; Code:

;; setup mode line to display less about minor modes
(require 'diminish)

;(when (require 'diminish nil 'noerror)
;  (eval-after-load "abbrev"
;    (diminish 'abbrev-mode "Ab"))
;  (eval-after-load "yasnippet"
;    (diminish 'yas-minor-mode "ys"))
;  (eval-after-load "flyspell"
;    (diminish 'flyspell-mode "Fl")))

(diminish 'abbrev-mode)
(diminish 'yas-minor-mode)
(diminish 'flyspell-mode)
(diminish 'projectile-mode)
(diminish 'git-gutter+-mode)
(diminish 'smartparens-mode)
(diminish 'subword-mode)
(diminish 'whole-line-or-region-local-mode)
(diminish 'counsel-mode)
(diminish 'ivy-mode)
(diminish 'which-key-mode)
(diminish 'eldoc-mode)
(diminish 'auto-fill-function)
(diminish 'org-indent-mode)
(diminish 'orgtbl-mode)
(diminish 'typo-mode)
(diminish 'gcmh-mode)
(diminish 'visual-line-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "el")))

(provide 'adh_diminish)

;;; adh_diminish.el ends here
