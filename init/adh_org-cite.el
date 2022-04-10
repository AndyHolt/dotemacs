;;; adh_org-cite.el --- Set up org cite -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 09 Oct 2021 22:58
;; URL: https://github.com/AndyHolt/dotemacs/blob/main/init/adh_org-cite.el

;;; Commentary:
;;
;; From Org 9.5, org-cite functionality (oc.el) provides the framework for
;; citations, including their export. The basic configuration provided is
;; minimal, designed for extensions to be built upon it.
;;
;; My main needs are for latex export and html export. For latex, export using
;; biblatex (this is supposed to be a built in package)

;;; Code:

(require 'oc-biblatex)
(require 'oc-csl)

(setq org-cite-export-processors '((latex biblatex)
                                   (html csl)
                                   (t basic)))

(setq org-cite-biblatex-options "style=verbose-ibid")

(setq org-cite-csl-styles-dir "~/csl")
(setq org-cite-csl--fallback-style-file "~/csl/chicago-note-bibliography.csl")

(setq bibtex-completion-format-citation-functions
      '((org-mode . bibtex-completion-format-citation-org-cite)
        (latex-mode . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
        (rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
        (default . bibtex-completion-format-citation-default)))

(provide 'adh_org-cite)
;;; adh_org-cite.el ends here
