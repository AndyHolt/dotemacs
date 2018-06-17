;;; adh_latex.el --- Settings for editing LaTeX

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 29 Dec 2014 16:02
;; URL: https://github.com/AndyHolt/dotemacs/blob/master/init/adh_latex.el

;;; Commentary:
;;
;; Set up AUCTeX and RefTeX and other modes for LaTeX editing.

;;; Code:

;; make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; enable PDFLaTeX for all documents
(setq TeX-PDF-mode t)

;; enable RefTeX - for managing cross references, bibliographies, indices,
;; document navigation etc
; [todo] - enable RefTeX (this might be done already in mode file)

;; enable shell-escape when compiling TeX documents
;;   allows calling of external commands from within latex packages
(add-hook 'TeX-mode-hook
  (lambda ()
    (setq TeX-command-extra-options "-shell-escape")))

(eval-after-load "latex"
  '(progn
     (define-key LaTeX-mode-map (kbd "C-{") 'helm-bibtex)))

(provide 'adh_latex)

;;; adh_latex.el ends here
