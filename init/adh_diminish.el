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

(eval-after-load "abbrev-mode"
  '(diminish 'abbrev-mode))
(eval-after-load "yas-minor-mode"
  '(diminish 'yas-minor-mode))
(eval-after-load "flyspell-mode"
  '(diminish 'flyspell-mode))
(eval-after-load "projectile-mode"
  '(diminish 'projectile-mode))
(eval-after-load "git-gutter+-mode"
  '(diminish 'git-gutter+-mode))
(eval-after-load "smartparens-mode"
  '(diminish 'smartparens-mode))
(eval-after-load "subword-mode"
  '(diminish 'subword-mode))
(eval-after-load "whole-line-or-region-local-mode"
  '(diminish 'whole-line-or-region-local-mode))
(eval-after-load "whole-line-or-region-global-mode"
  '(diminish 'whole-line-or-region-global-mode))
(eval-after-load "counsel-mode"
  '(diminish 'counsel-mode))
(eval-after-load "ivy-mode"
  '(diminish 'ivy-mode))
(eval-after-load "which-key-mode"
  '(diminish 'which-key-mode))
(eval-after-load "eldoc-mode"
  '(diminish 'eldoc-mode))
(eval-after-load "auto-fill-function"
  '(diminish 'auto-fill-function))
(eval-after-load "org-indent-mode"
  '(diminish 'org-indent-mode))
(eval-after-load "orgtbl-mode"
  '(diminish 'orgtbl-mode))
(eval-after-load "typo-mode"
  '(diminish 'typo-mode))
(eval-after-load "gcmh-mode"
  '(diminish 'gcmh-mode))
(eval-after-load "visual-line-mode"
  '(diminish 'visual-line-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "el")))

(provide 'adh_diminish)
;;; adh_diminish.el ends here
