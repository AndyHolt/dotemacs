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

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:color amaranth)
   "window"
   ("h" windmove-left "left")
   ("j" windmove-down "down")
   ("k" windmove-up "up")
   ("l" windmove-right "right")
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horiz")
   ("^" enlarge-window "taller")
   ("%" shrink-window "shorter")
   ("}" enlarge-window-horizontally "wider")
   ("{" shrink-window-horizontally "narrow")
   ("t" transpose-frame "transpose")
   ("o" delete-other-windows "one" :color blue)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-maximise-window "ace-one" :color blue)
   ("b" ido-switch-buffer "buf")
   ("m" headlong-bookmark-jump "bmk")
   ("q" nil "cancel")))
        

(provide 'adh_hydra)

;;; adh_hydra.el ends here
