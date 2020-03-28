;;; adh_pdf.el --- Set up pdf-tools and other pdf related things -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 05 Mar 2018 18:24
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Set up the pdf-tools package, a much more powerful pdf viewer for emacs,
;;including the ability to annotate files, search within pdfs, etc.

;;; Code:

;; installation/setup
(pdf-loader-install)

;; When opening pdfs, show whole page as default
(setq-default pdf-view-display-size 'fit-page)

;; use normal isearch, pdf-tools can't use swiper 
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

;; use C-w and M-w
(define-key pdf-view-mode-map (kbd "C-w") 'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save)
(define-key pdf-view-mode-map (kbd "w") 'pdf-view-kill-ring-save)

(provide 'adh_pdf)
;;; adh_pdf.el ends here
