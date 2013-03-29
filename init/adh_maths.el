;;; contains some function definitions for doing maths in elisp

(defun factorial (n)
  "Computes the factorial of a number, n!"
  (interactive)
  (if (<= n 1)
      1
    (* n (factorial (- n 1)))))

(defun pow (x n)
  "computes x to power n (x^n)
   currently only for integers
   todo: extend for non-integers"
  (if (= n 1)
      x
    (* x (pow x (- n 1)))))

(provide 'adh_maths)
