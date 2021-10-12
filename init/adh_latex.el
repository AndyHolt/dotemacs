;;; adh_latex.el --- Settings for editing LaTeX -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 29 Dec 2014 16:02
;; URL: https://github.com/AndyHolt/dotemacs/blob/master/init/adh_latex.el

;;; Commentary:
;;
;; Set up AUCTeX and RefTeX and other modes for LaTeX editing.

;;; Code:

;; Load AUCTeX when tex is loaded
(eval-after-load "tex-mode"
  '(require 'latex))

;; make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
; by default, assume current file is master, don't prompt for master
(setq-default TeX-master t)

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

;; add run-latex script to AUCTeX compile options
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("run-latex" "run-latex %t" TeX-run-command t t )))

(defun adh-set-default-TeX-command ()
  "Set default TeX command to run-latex. To be used in LaTeX mode
hook."
  (setq TeX-command-default "run-latex"))
(add-hook 'LaTeX-mode-hook #'adh-set-default-TeX-command)

(provide 'adh_latex)
;;; adh_latex.el ends here
