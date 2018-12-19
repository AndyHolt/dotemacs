;;; adh_org.el --- Org mode setup

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:03
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Setup for org mode.

;;; Code:

(require 'org)

;; ensure files with .org extension use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; have *todo* buffer use org mode
;(add-to-list 'auto-mode-alist '("*todo*" . org-mode))

;; add Markdown exporting for org files
(require 'ox-md)

(defvar adh-dropbox-location
  (cond ((eq system-type 'gnu/linux) "~/Dropbox/")
        ((eq system-type 'windows-nt) "d:/Dropbox/")
        (t "~/Dropbox/"))
  "Holds the location of the Dropbox root directory for the current system.")

;; use org-mac to get links from mac applications
; (require 'org-mac-link)

;; add files to org agenda
(setq org-agenda-files
      (list (concat adh-dropbox-location "Org_files/todo.org")
            (concat adh-dropbox-location "Org_files/gcal.org")
            (concat adh-dropbox-location "Org_files/uccf-cal.org")
            (concat adh-dropbox-location "Org_files/church-cal.org")
            (concat adh-dropbox-location "Org_files/notes.org")
            (concat adh-dropbox-location "Org_files/diary.org")))

;; set diary for inclusion in agenda
(setq org-agenda-include-diary t)

;; set up diary file
(setq org-agenda-diary-file (concat adh-dropbox-location "Org_files/diary.org"))

;; TODO keyword states for org files
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "IN PROGRESS" "WAITING" "|" "INACTIVE" "CANCELLED" "DONE")))

;; global keybindings for org mode
(eval-after-load "org"
  '(progn
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)))

;; update parent cookies for checkboxes
;; from whattheemacsd.com
(defun adh-org-update-parent-cookie ()
  "Update parent cookies for checkboxes in org files."
  (interactive)
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  "Update checkboxes after killing a line."
  (adh-org-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  "Update checkboxes after killing a line."
  (adh-org-update-parent-cookie))

;; setup capture target file
(setq org-default-notes-file "~/Dropbox/Org_files/todo.org")

;; setup capture templates
;; OLD VERSION!
;; (setq org-capture-templates
;;   '(("t" "Templates for TODO items")
;;     ("tt" "Todo" entry (file+headline "~/Dropbox/Org_files/todo.org"
;;                                       "Tasks")
;;      "* TODO %?\n %i\n"
;;      :kill-buffer)
;;     ("tc" "Computer" entry (file+headline
;;                             "~/Dropbox/Org_files/computer_stuff.org"
;;                             "Unfiled")
;;      "* TODO %?\n %i\n"
;;      :kill-buffer)
;;     ("tw" "Work" entry (file "~/Dropbox/Org_files/Work.org")
;;      "* TODO %?\n %i\n"
;;      :kill-buffer)
;;    ("n" "Note" entry (file+headline "~/Dropbox/Org_files/todo.org"
;;                                     "Notes")
;;         "* %?\n %i\n %a")
;;    ("l" "Link" entry (file+headline "~/Dropbox/Org_files/todo.org"
;;                                "Web Links")
;;     "* %a\n %?\n %i")))

;; capture templates, experimental new one
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Org_files/todo.org" "Tasks")
         "* TODO %?\n %i\n\n")
        ("c" "Todo with Context" entry (file+headline "~/Dropbox/Org_files/todo.org" "Tasks")
         "* TODO %?\n %i\n%a\n\n")
        ("e" "Event")
        ("ep" "Event in Personal Calendar" entry (file "~/Dropbox/Org_files/gcal.org" )
         "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("eu" "Event in UCCF Calendar" entry (file "~/Dropbox/Org_files/uccf-cal.org" )
         "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("ec" "Event in Church Calendar" entry (file "~/Dropbox/Org_files/church-cal.org" )
         "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("r" "Reading")
        ("rb" "Book" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                          "Reading and study" )
         "* book: %^{author}, %^{title}\n%?\n%T\n\n")
        ("ra" "Article" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                             "Reading and study")
         "* article: %?\n%T\n\n")
        ("n" "Note" entry (file+datetree "~/Dropbox/Org_files/notes.org")
         "* %?\n\n%T\n\n")))

;; start org protocol - for creating links etc to external
;; applications
;(require 'org-protocol)

;(setq org-protocol-default-template-key "l")

;; set pdf application for opening links
;; from stack overflow
(cond ((eq system-type 'gnu/linux)
       (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))
      ((eq system-type 'darwin)
       (setcdr (assoc "\\.pdf\\'" org-file-apps) "open %s")))

;; setup refiling
;; refiling possible to:
;; - current buffer, up to 10 levels of headings
;; - agenda files, up to 3 levels of headings
;; - book notes files, up to 10 levels of headings (probably too much, but ah
;;   well!)
;; - notes files, up to 10 levels of headings
(setq adh-booknotes-files (file-expand-wildcards "~/Documents/BookNotes/*.org"))
(setq adh-notes-files (file-expand-wildcards "~/Dropbox/Notes/*.org"))
(setq adh-biblenotes-files (file-expand-wildcards "~/Documents/BibleNotes/*.org"))


(setq org-refile-targets '((nil :maxlevel . 10)
                           (org-agenda-files :maxlevel . 3)
                           (adh-booknotes-files :maxlevel . 10)
                           (adh-notes-files :maxlevel . 10)
                           (adh-biblenotes-files :maxlevel . 10)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; refiling into date-tree
;; - Primary use case is when note has been captured from mobile phone, and
;;   appended to inbox of org-files through dropbox. This note can later, within
;;   Emacs, be refiled to the appropriate place in date-tree of notes.org
;; - The motivation for refiling into date-tree, not just into a topical note
;;   system is for ease of finding when I can vaguely remember when something
;;   was noted, but not any details for searching
;; - Code borrowed from
;;   https://emacs.stackexchange.com/questions/10597/how-to-refile-into-a-datetree
(defun adh-org-read-datetree-date (d)
  "Parse a time string D and return a date to pass to the datetree functions."
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))

(defun adh-org-refile-to-notes (&optional bfn)
  "Refile an entry to a datetree under an archive."
  (interactive)
  (require 'org-datetree)
  (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/Dropbox/Org_files/notes.org"))))
         (datetree-date (or (if (org-entry-get nil "TIMESTAMP" t)
                                (adh-org-read-datetree-date (org-entry-get nil "TIMESTAMP" t))
                              nil)
                            (adh-org-read-datetree-date (org-read-date t nil)))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil
                              (with-current-buffer bfn
                                (save-excursion
                                  (org-datetree-find-date-create datetree-date)
                                  (point)))))))
(define-key org-mode-map (kbd "C-c m n") 'adh-org-refile-to-notes)

;; set up helm-org-rifle
(defun adh-search-notes ()
    "Call 'helm-org-rifle' with note files from 'adh-booknotes-files and
'adh-notes-files"
  (interactive)
  (helm-org-rifle-files (append adh-biblenotes-files
                                adh-booknotes-files
                                adh-notes-files
                                org-agenda-files)))

(global-set-key (kbd "C-c m f") 'adh-search-notes)


;; when inserting new headline, insert below content of current
;; headline
(setq org-insert-heading-respect-content t)

;; setup languages for Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   ;(c++ . t)
   (python . t)
   (latex . t)
   (octave .t)
   (haskell . t)
   (R . t)
   (dot . t)
   (gnuplot . t)
   (sql . t)))

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; set up mobile org
(setq org-directory (concat adh-dropbox-location "Org_files"))
(setq org-mobile-directory (concat adh-dropbox-location "Apps/MobileOrg"))
(setq org-mobile-inbox-for-pull (concat adh-dropbox-location "Org_files/todo.org"))

;; Make RefTeX work with Org-Mode
;; use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
  (load-library "reftex") 
  (and (buffer-file-name)
  (file-exists-p (buffer-file-name))
  (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; RefTeX formats for biblatex (not natbib)
(setq reftex-cite-format
      '(
        (?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}")
        ))

(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("autocite" "[{")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{") 
        ("citetitle" "[{") 
        ("citetitles" "[{") 
        ("headlessfullcite" "[{")))

(setq reftex-cite-prompt-optional-args nil)
(setq reftex-cite-cleanup-optional-args t)

;; add adharticle class to org's recognised classes
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("adharticle"
               "\\documentclass{adharticle}
               [NO-DEFAULT-PACKAGES]
               [PACKAGES]
               [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("adhhandout"
               "\\documentclass{adhhandout}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; (setq org-latex-pdf-process
;;       "latexmk -pdflatex='-shell-escape' -pdf -f %f")

;; set up org-ref
(setq org-ref-default-citation-link "autocite")
(setq reftex-default-bibliography '("~/Projects/WritingTools/Theology.bib"))
;; not yet setup up fully, not sure if I need it...
;; (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(setq org-ref-default-bibliography "~/Projects/WritingTools/Theology.bib")
(setq org-ref-notes-function 'org-ref-notes-function-many-files)
(setq org-ref-notes-directory "~/Documents/BookNotes")

(require 'org-ref)

;; use interleave mode
(require 'interleave)

;; correct quote marks when exporting
(setq org-export-with-smart-quotes t)

;; highlight current line while in agenda view
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; when archiving elements, preserve hierarchy structure in archieve
;; From https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

(provide 'adh_org)

;;; adh_org.el ends here
