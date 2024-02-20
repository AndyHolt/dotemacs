;;; adh_magit.el --- set up for Magit -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 29 Dec 2014 18:00
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up for Magit - an Emacs mode for git.

;;; Code:

(autoload 'magit-status "magit" "" t)
(global-set-key (kbd "C-c m s") 'magit-status)

;; Hydra for Smerge commands

(defhydra hydra-smerge (:color amaranth
                               :hint nil)
  "
^Navigation^    ^Select^              ^Other^                   ^Quit^
^^^^^^^^^^------------------------------------------------------------------------
_n_: next       _u_: keep upper       _C_: Combine with next    _q_: quit
_p_: prev       _l_: keep lower       _E_: smerge Ediff         _C-l_: recenter
^ ^             _a_: keep all         _R_: Refine
^ ^             _RET_: keep current   _r_: resolve
^ ^             _b_: keep base        _<_: diff base upper
^ ^             _m_: keep upper       _>_: diff base lower
^ ^             _o_: keep lower
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("b" smerge-keep-base)
  ("m" smerge-keep-upper)
  ("o" smerge-keep-lower)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-upper)
  (">" smerge-diff-base-lower)
  ("q" nil :color blue)
  ("C-l" recenter-top-bottom)
  )

(with-eval-after-load "smerge-mode"
  (keymap-set smerge-mode-map "C-c ^" 'hydra-smerge/body))

(provide 'adh_magit)
;;; adh_magit.el ends here
