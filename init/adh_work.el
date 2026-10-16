;;; adh_work.el --- Work machine config -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Wed 17 Jun 2026 16:51

;;; Commentary:
;;
;; Commentary

;;; Code:

;; Override values from adh_mode.el
(setq bibtex-completion-library-path
      (expand-file-name "~/Documents/References/pdfs"))
(setq bibtex-completion-notes-path
      (expand-file-name "~/Documents/References/notes"))
(setq bibtex-completion-bibliography
      (expand-file-name "~/Documents/References/bibliography.bib"))

(provide 'adh_work)
;;; adh_work.el ends here
