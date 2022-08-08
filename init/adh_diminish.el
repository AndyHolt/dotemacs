;;; adh_diminish.el --- Clean up modeline clutter -*- lexical-binding: t -*-

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

(eval-after-load 'abbrev
  '(diminish 'abbrev-mode))
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode))
(eval-after-load 'flyspell
  '(diminish 'flyspell-mode))
;; (eval-after-load 'projectile
;;   '(diminish 'projectile-mode))
(eval-after-load 'git-gutter-fringe+
  '(diminish 'git-gutter+-mode))
(eval-after-load 'smartparens
  '(diminish 'smartparens-mode))
(eval-after-load 'subword
  '(diminish 'subword-mode))
(eval-after-load 'whole-line-or-region
  '(progn
     (diminish 'whole-line-or-region-local-mode)
     (diminish 'whole-line-or-region-global-mode)))
(eval-after-load 'counsel
  '(diminish 'counsel-mode))
(eval-after-load 'ivy
  '(diminish 'ivy-mode))
(eval-after-load 'which-key
  '(diminish 'which-key-mode))
(eval-after-load 'eldoc
  '(diminish 'eldoc-mode))
(eval-after-load 'simple
  '(diminish 'auto-fill-function))
(eval-after-load 'org-indent
  '(diminish 'org-indent-mode))
(eval-after-load 'org-table
  '(diminish 'orgtbl-mode))
;; (eval-after-load 'typo
;;   '(diminish 'typo-mode))
(eval-after-load 'gcmh
  '(diminish 'gcmh-mode))
(eval-after-load 'simple
  '(diminish 'visual-line-mode))
(eval-after-load 'highlight-indent-guides
  '(diminish 'highlight-indent-guides-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "el")))

(provide 'adh_diminish)
;;; adh_diminish.el ends here
