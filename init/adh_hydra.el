;;; adh_hydra.el --- set up hydra package -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 30 May 2015 12:05
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Hydra provides a modal element to emacs, which allows use of repeated
;; keybindings more easily.

;;; Code:

(require 'hydra)

(defun hydra-universal-arugment (arg)
  (interactive "P")
  (setq prefix-arg (if (consp arg)
                       (list (* 4 (car arg)))
                     (if (eq arg '-)
                         (list -4)
                       '(4)))))

(defhydra hydra-window (:color amaranth
                        :hint nil)
  "
^Select^    ^Split^          ^Resize^        ^Other^             ^Quit^
^^^^^^^^^^------------------------------------------------------------------------
_h_: left   _v_: vertical    _H_: narrower   _t_: transpose      _q_: quit
_j_: down   _x_: horizontal  _J_: taller     _o_: one
_k_: up     ^ ^              _K_: shorter    _a_: ace select
_l_: right  ^ ^              _L_: wider      _s_: ace swap
^ ^         ^ ^              ^ ^             _d_: ace delete
^ ^         ^ ^              ^ ^             _i_: ace one
^ ^         ^ ^              ^ ^             _b_: switch buffer
^ ^         ^ ^              ^ ^             _m_: bookmarks
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("J" enlarge-window)
  ("K" shrink-window)
  ("L" enlarge-window-horizontally)
  ("H" shrink-window-horizontally)
  ("t" transpose-frame)
  ("o" delete-other-windows :color blue)
  ("a" ace-window)
  ("s" ace-swap-window)
  ("d" ace-delete-window)
  ("i" ace-maximise-window :color blue)
  ("b" ido-switch-buffer)
  ("m" headlong-bookmark-jump)
  ("q" nil :color blue)
  )

(global-set-key (kbd "C-M-o") 'hydra-window/body)


(provide 'adh_hydra)
;;; adh_hydra.el ends here
