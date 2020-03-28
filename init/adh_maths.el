;;; adh_maths.el --- Maths functions for elisp -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:57
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Some mathematical functions for use in Emacs Lisp.

;;; Code:

(defun factorial (n)
  "Computes the factorial of a number, N!"
  (interactive)
  (if (<= n 1)
      1
    (* n (factorial (- n 1)))))

;; [todo] -  extend for non-integers"
(defun pow (x n)
  "Computes X to power N (X^N)."
  (if (= n 1)
      x
    (* x (pow x (- n 1)))))

(provide 'adh_maths)

;;; adh_maths.el ends here
