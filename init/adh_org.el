;;; adh_org.el --- Org mode setup -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:03
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Setup for org mode.

;;; Code:

(with-timer "loading org itself"
(require 'org)
)

(with-timer "auto-mode setup for org"
;; ensure files with .org extension use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
)

;; have *todo* buffer use org mode
;(add-to-list 'auto-mode-alist '("*todo*" . org-mode))

(with-timer "load ox-md"
;; add Markdown exporting for org files
; (require 'ox-md)
)

(with-timer "set org files and directories"
(defvar adh-dropbox-location
  (cond ((eq system-type 'darwin) (expand-file-name "~/Dropbox/"))
        ((eq system-type 'gnu/linux) (expand-file-name "~/Dropbox/"))
        ((eq system-type 'windows-nt) "d:/Dropbox/")
        (t "~/Dropbox/"))
  "Holds the location of the Dropbox root directory for the current system.")

;; set org files directory
(setq org-directory (expand-file-name "Org_files/" adh-dropbox-location))

;; use org-mac to get links from mac applications
; (require 'org-mac-link)

;; add files to org agenda
(setq adh-non-cal-org-agenda-files
      (list (expand-file-name "todo.org" org-directory)
            (expand-file-name "notes.org" org-directory)
            (expand-file-name "diary.org" org-directory)
            ))
(setq adh-cal-agenda-files
      (list (expand-file-name "cal.org" org-directory)
            (expand-file-name "fam-cal.org" org-directory)
            (expand-file-name "sbts-cal.org" org-directory)
            (expand-file-name "church-cal.org" org-directory)
            (expand-file-name "cbc-cal.org" org-directory)
            ))

(require 'dash)
(setq org-agenda-files
      (-concat adh-non-cal-org-agenda-files
               adh-cal-agenda-files
               ))

;; set diary for inclusion in agenda
(setq org-agenda-include-diary t)

;; set up diary file
(setq org-agenda-diary-file (expand-file-name "diary.org" org-directory))

;; TODO keyword states for org files
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN PROGRESS(p)" "WAITING(s)" "|"
                  "INACTIVE(i)" "CANCELLED(c)" "DONE(d)")))

;; Save log change notes into drawer (keep out the way most of the time)
(setq org-log-into-drawer "LOGBOOK")
)

(with-timer "org keybindings and other functions"
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
)

(with-timer "org capture setup"
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
      '(("t" "Todo" entry (file+headline "~/Dropbox/Org_files/todo.org" "Inbox")
         "* TODO %?\n%i\n%U\n")
        ("c" "Todo with Context" entry (file+headline "~/Dropbox/Org_files/todo.org" "Inbox")
         "* TODO %?\n%i\n%a\n%U\n")
        ;; ("e" "Event")
        ;; ("ep" "Event in Personal Calendar" entry (file "~/Dropbox/Org_files/gcal.org" )
        ;; "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;; ("eu" "Event in UCCF Calendar" entry (file "~/Dropbox/Org_files/uccf-cal.org" )
        ;;  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;; ("ec" "Event in Church Calendar" entry (file "~/Dropbox/Org_files/church-cal.org" )
        ;;  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;; ("ef" "Event in Family Calendar" entry (file "~/Dropbox/Org_files/family-cal.org" )
        ;; "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;; ("es" "Event in SBTS Calendar" entry (file "~/Dropbox/Org_files/sbts-cal.org" )
        ;; "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("s" "SBTS")
        ("sh" "Hebrew" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                            "20440 Hebrew Syntax and Exegesis")
         "* TODO %?\nDEADLINE: %^T\n%U\n")
        ("sp" "Preaching" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                           "30000 Christian Preaching")
         "* TODO %?\nDEADLINE: %^t\n%U\n")
        ("sc" "Biblical Counselling" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                           "34300WW Intro to Biblical Counselling")
         "* TODO %?\nDEADLINE: %^T\n%U\n")
        ("sj" "Cromwell Journal" entry
          (file+olp+datetree "/Users/adh/Documents/SBTS/SU21_25120-ChurchHistory2/SpiritualityPaper/CH2-Cromwell-Journal.org")
         "* %?\n\n%T\n\n")
        ("r" "Reading")
        ("rb" "Book" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                          "Reading and study" )
         "* TODO book: %^{author}, %^{title}\n%?\n%T\n\n")
        ("ra" "Article" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                             "Reading and study")
         "* TODO article: %?\n%T\n\n")
        ("n" "Note" entry (file+olp+datetree "~/Dropbox/Org_files/notes.org")
         "* %?\n\n%T\n\n")
        ("l" "Link from Browser" entry (file+headline "todo.org" "Inbox")
         (function adh-org-capture-browser-tabs))
        ("b" "Bread journal" entry (file+olp+datetree
                                    "/Users/adh/Documents/notes/notes/bread-notebook.org"
                                    "Notebook")
         "* %?\n\n%U\n")))

;; define function for use in capture to get tabs via helm-browser-tabs
(defun adh-org-capture-browser-tabs ()
    "Get browser tabs from helm (one or multiple) and generate a org capture
template to capture them."
    (let* ((tabs-list (with-temp-buffer (org-mode)
                                        (helm-browser-tabs t)
                                        (buffer-substring-no-properties (point-min)
                                                                        (point-max))))
           (capture-headline (if (string-match "\n" tabs-list)
                                 "Links relating to %?"
                               (format "%s%%?"
                                       (progn
                                         (string-match org-link-bracket-re
                                                       tabs-list)
                                         (or (match-string 3 tabs-list)
                                             (match-string 1 tabs-list)))))))
      (format "* TODO %s\n%s\n%%T\n" capture-headline tabs-list)))
)
;; start org protocol - for creating links etc to external
;; applications
;(require 'org-protocol)

;(setq org-protocol-default-template-key "l")

(with-timer "org pdf set up"
;; set pdf application for opening links
;; from stack overflow
(cond ((eq system-type 'gnu/linux)
       (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))
      ((eq system-type 'darwin)
       (setcdr (assoc "\\.pdf\\'" org-file-apps) "open %s")))
)

(with-timer "org refile"
;; setup refiling
;; refiling possible to:
;; - current buffer, up to 10 levels of headings
;; - agenda files, up to 3 levels of headings
;; - book notes files, up to 10 levels of headings (probably too much, but ah
;;   well!)
;; - notes files, up to 10 levels of headings

;; Try changing refile targets from all agenda files to only agenda files which
;; aren't calendar files. I don't have to move things to calendar files often at
;; all (I only really do capturing to files) so it's annoying when trying to
;; refile to a heading in todo.org and suggestions from the associated calendar
;; file are suggested instead.
;; [todo] - if this works well, streamline setting of org-agenda-files from
;; different groups, made up of non-calendar agenda files and calendar agenda
;; files. That will save from having redudency between variables that need to be
;; modifed in different places. THIS IS CURRENTLY A BOTCH!
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache t)

(defun adh-init-notes-files-and-refile-targets ()
  "Set org files for notes and set up refile targets.

This function can be used both to initialise the notes files and targets an
initialisation, but also for adding new files. Calling this function after
creating a new file in one of the standard notes directories will ensure that
that new file is included in notes targets."
  (interactive)
  (setq adh-booknotes-files (file-expand-wildcards
                             "~/Documents/notes/book-notes/*.org"))
  (setq adh-notes-files (file-expand-wildcards "~/Documents/notes/notes/*.org"))
  (setq adh-biblenotes-files (file-expand-wildcards
                              "~/Documents/notes/bible-notes/*.org"))
  (setq adh-sbtsnotes-files (file-expand-wildcards
                             "~/Documents/notes/sbts-notes/*.org"))
  (setq org-refile-targets '((nil :maxlevel . 10)
                             (adh-non-cal-org-agenda-files :maxlevel . 5)
                             (adh-booknotes-files :maxlevel . 10)
                             (adh-notes-files :maxlevel . 10)
                             (adh-biblenotes-files :maxlevel . 10)
                             (adh-sbtsnotes-files :maxlevel . 10)))
  (org-refile-cache-clear))

(eval-after-load 'org
  '(progn
     (require 'org-refile)
     (adh-init-notes-files-and-refile-targets)))

;; When refiling, make a link to the original location in the annotation
;; (defun adh-org-refile-get-orig-location ()
;;     "Store org-link with the location of the original note, when refiling.

;; Function to be called when beginning org-refile, so as to have link ready to be
;;   inserted when refiling link."
;;   (interactive)
;;   (call-interactively 'org-store-link))
(defvar adh-org-refile-parent nil
  "Parent of an org entity about to be refiled")

(advice-add 'org-refile
            :before
            (lambda (&rest _)
              (setq adh-org-refile-parent (adh-get-org-headline-location)))
            '((name . adh-advice-org-refile-get-orig-location)))

(defun adh-get-org-headline-location ()
    "Get the location of the current headline, either the
     headline a level above the current headline, or else the
     file name"
    (interactive)
    (save-excursion
      (if (org-up-heading-safe)
          (org-link-make-string (concat "id:" (org-id-get-create)) (org-get-heading t t t t))
        (file-name-nondirectory (buffer-file-name)))))

(defun adh-org-refile-insert-orig-location ()
    "Insert a link to log note back to original location of refiled note."
  (interactive)
  (insert (format "Refiled from %s"
                  adh-org-refile-parent)))


(add-hook 'org-log-buffer-setup-hook 'adh-org-refile-insert-orig-location)

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
  "Refile an entry to notes file and appropriate datetree location."
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
(define-key org-mode-map (kbd "C-c m n") #'adh-org-refile-to-notes)

;; Set up a datetree-refile for Fuller Journal (HOTB course)
(defun adh-org-refile-to-fuller-journal ()
  "Refile an entry to notes file and appropriate datetree location."
  (interactive)
  (require 'org-datetree)
  (let* ((bfn (find-file-noselect (expand-file-name
                                   (concat
                                    "~/Documents/SBTS/F20_26100-HistoryOfTheBaptists/"
                                    "FullerPaper/HOTB-Fuller-Journal.org"))))
         (datetree-date (or (if (org-entry-get nil "TIMESTAMP" t)
                                (adh-org-read-datetree-date (org-entry-get nil "TIMESTAMP" t))
                              nil)
                            (adh-org-read-datetree-date (org-read-date t nil)))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil
                              (with-current-buffer bfn
                                (save-excursion
                                  (org-datetree-find-date-create datetree-date)
                                  (point)))))))
)

(with-timer "org-rifle and search"
;; set up helm-org-rifle
;; (require 'helm-org-rifle)
(autoload 'helm-org-rifle "helm-org-rifle" "" t)
(autoload 'helm-org-rifle-files "helm-org-rifle" "" t)

(defun adh-search-notes ()
    "Call `helm-org-rifle' with note files from `adh-booknotes-files' and
`adh-notes-files'"
  (interactive)
  (helm-org-rifle-files (append adh-biblenotes-files
                                adh-booknotes-files
                                adh-notes-files
                                org-agenda-files
                                adh-sbtsnotes-files)))

(global-set-key (kbd "C-c m f") #'adh-search-notes)
)

(with-timer "org insert headings set up"
;; todo: move this to a general settings section!
;; when inserting new headline, insert below content of current
;; headline
(setq org-insert-heading-respect-content t)
)

(with-timer "babel"
;; setup languages for Org babel
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (latex . t)
;;    (python . t)
;;    (shell . t)
;;    (sql . t)
;;   ))

(autoload 'org-babel-execute:emacs-lisp "ob-emacs-lisp")
(autoload 'org-babel-expand-body:emacs-lisp "ob-emacs-lisp")

(autoload 'org-babel-execute:latex "ob-latex")
(autoload 'org-babel-expand-body:latex "ob-latex")

(autoload 'org-babel-execute:python "ob-python")

(autoload 'org-babel-execute:sh "ob-shell")
(autoload 'org-babel-expand-body:sh "ob-shell")
(autoload 'org-babel-execute:shell "ob-shell")
(autoload 'org-babel-expand-body:shell "ob-shell")
(autoload 'org-babel-execute:bash "ob-shell")
(autoload 'org-babel-expand-body:bash "ob-shell")
(autoload 'org-babel-execute:zsh "ob-shell")
(autoload 'org-babel-expand-body:zsh "ob-shell")

(autoload 'org-babel-execute:sql "ob-sql")
(autoload 'org-babel-expand-body:sql "ob-sql")

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; set correct commands for evaluation of org babel blocks
(setq org-babel-python-command "python3")
)

(with-timer "org tex-y stuff"
;; Make RefTeX work with Org-Mode
;; use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(with-timer "reftex set up"
; (require 'reftex)
; (autoload 'helm-bibtex "reftex")

;; (defun org-mode-reftex-setup ()
;;   (setq TeX-master t)
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (reftex-parse-all)))

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; ;; RefTeX formats for biblatex (not natbib)
;; (setq reftex-cite-format
;;       '(
;;         (?\C-m . "\\cite[]{%l}")
;;         (?t . "\\textcite{%l}")
;;         (?a . "\\autocite[]{%l}")
;;         (?p . "\\parencite{%l}")
;;         (?f . "\\footcite[][]{%l}")
;;         (?F . "\\fullcite[]{%l}")
;;         (?x . "[]{%l}")
;;         (?X . "{%l}")
;;         ))

;; (setq font-latex-match-reference-keywords
;;       '(("cite" "[{")
;;         ("cites" "[{}]")
;;         ("autocite" "[{")
;;         ("footcite" "[{")
;;         ("footcites" "[{")
;;         ("parencite" "[{")
;;         ("textcite" "[{")
;;         ("fullcite" "[{") 
;;         ("citetitle" "[{") 
;;         ("citetitles" "[{") 
;;         ("headlessfullcite" "[{")))

;; (setq reftex-cite-prompt-optional-args nil)
;; (setq reftex-cite-cleanup-optional-args t)
)

(with-timer "org latex classes and export"
;; Org to LaTeX export class setup
(add-to-list 'org-latex-packages-alist
             '("normalem" "ulem" nil))

; (require 'ox-latex)
(autoload 'org-export-dispatch "ox-latex" "" t)

(eval-after-load "ox-latex"
  '(progn
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

    (add-to-list 'org-latex-classes
		 '("sbtspaper"
		   "\\documentclass{sbtspaper}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("sbtsprecis"
		   "\\documentclass{sbtsprecis}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("sbtshistread"
		   "\\documentclass{sbtshistread}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\lecture{%s}" . "\\lecture*{%s}")
		   ("\\question{%s}" . "\\question*{%s}")
		   ("\\subquestion{%s}" . "\\subquestion*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("sbtsreview"
		   "\\documentclass{sbtsreview}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("adhdoc"
		   "\\documentclass{adhdoc}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("adhsernotes"
		   "\\documentclass{adhsernotes}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("adhseroutline"
		   "\\documentclass{adhseroutline}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
		 '("adhsertrans"
		   "\\documentclass{adhsertrans}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRAS]"
		   ("\\section{%s}" . "\\section*{%s}")
		   ("\\subsection{%s}" . "\\subsection*{%s}")
		   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		   ("\\paragraph{%s}" . "\\paragraph*{%s}")
		   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


    (add-to-list 'org-latex-classes
		 '("sbtsaec"
		   "\\documentclass{sbtsaec}
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

    ;; set the command used for compiling latex files
    (setq org-latex-pdf-process '("run-latex -p %latex -o %o %f")
          org-latex-compiler "xelatex")
    ))
)

(with-timer "org-ref set up"
;; set up org-ref
(setq org-ref-default-citation-link "autocite")
(setq reftex-default-bibliography '("~/Projects/WritingTools/Theology.bib"))
;; not yet setup up fully, not sure if I need it...
;; (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(setq org-ref-default-bibliography "~/Projects/WritingTools/Theology.bib")
(setq org-ref-notes-function 'org-ref-notes-function-many-files)
(setq org-ref-notes-directory "~/Documents/notes/book-notes")

(with-timer "requiring org-ref"
;;(require 'org-ref)
(autoload 'org-ref-show-link-messages "org-ref")

(defun adh-require-org-ref-in-helm-bibtex (&rest args)
    "Ensure org-ref is loaded before calling helm-bibtex.
args are ignored."
    (require 'org-ref))

(advice-add 'helm-bibtex :before #'adh-require-org-ref-in-helm-bibtex)

)

;; change font used for org-ref links
;; (eval-after-load "org-ref"
;;   (set-face-attribute 'org-ref-cite-face nil :weight 'light :foreground "#f1d49b"
;;                       :underline nil)
;;   )
)

;; use interleave mode
; (require 'interleave)

;; correct quote marks when exporting
(setq org-export-with-smart-quotes t)

;; allow setting variable for individual documents using
;; "#+BIND: variable value" keyword settings in header/document
(setq org-export-allow-bind-keywords t)

;; highlight current line while in agenda view
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
)

(with-timer "archiving"
;; when archiving elements, preserve hierarchy structure in archive
;; From https://fuco1.github.io/2017-04-20-Archive-subtrees-under-the-same-hierarchy-as-original-in-the-archive-files.html
(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile  (car (org-archive--compute-location
		       (or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location))))
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

;; by default, org-mode highlights bold or italic text over a single new
;; line. Change that to 20 lines
(setcar (nthcdr 4 org-emphasis-regexp-components) 40)
)

(with-timer "org-publish"
;; setup org-publish
;; 
;; org-publish setup for notes to apple notes.app, for reading on phone
;; 
;; config is a modifed version of export for unites here:
;; https://vxlabs.com/2018/10/29/importing-orgmode-notes-into-apple-notes
;; (defun org-html-publish-to-html-for-apple-notes (plist filename pub-dir)
;;   "Convert blank lines to <br /> and remove <h1> titles."
;;   ;; temporarily configure export to convert math to images because
;;   ;; apple notes obviously can't use mathjax (the default)
;;   (let* ((org-html-with-latex 'imagemagick)
;;          (outfile
;;           (org-publish-org-to 'html filename
;;                               (concat "." (or (plist-get plist :html-extension)
;;                                               org-html-extension
;;                                               "html"))
;;                               plist pub-dir)))
;;     ;; 1. apple notes handles <p> paras badly, so we have to replace all blank
;;     ;;    lines (which the orgmode export accurately leaves for us) with
;;     ;;    <br /> tags to get apple notes to actually render blank lines between
;;     ;;    paragraphs
;;     ;; 2. remove large h1 with title, as apple notes already adds <title> as
;;     ;; the note title
;;     (shell-command
;;      (format "sed -i \"\" -e 's/^$/<br \\/>/' -e 's/<h1 class=\"title\">.*<\\/h1>$//' %s"
;;              outfile))
;;     outfile))

;; function to import published html for apple notes into notes app.
;; This will automatically sync with phone and any other devices.
;; Uses a keyboard maestro macro to do the import. Less neat than using
;; applescript, but it's a quick and dirty solution that works well. Getting
;; applescript to work as well would be very tricky!
;; (defun adh-publish-import-to-notes-app (plist)
;;     "Run Keyboard Maestro macro to import html notes from publishing-directory
;; to notes.app."
;;     (shell-command "osascript ~/Projects/Zenodotus/import-to-notes-app.scpt"))

;; function to help with cleaning up buffers before exporting project
(defun adh-kill-if-visiting (files)
    "Kill buffers visiting FILES.

FILES may be a file path, or a list of file paths, each of which
will be killed in turn.

For FILE (if a file path) or each file in FILES (if a list of
file paths), check if there is a buffer visiting the file and
kill it if there is. Any files which do not have a buffer
visiting them will not be visited"
    (let ((kill-fun (lambda (file)
                      (if-let ((buffer-name (find-buffer-visiting file)))
                          (kill-buffer buffer-name)))))
      (if (listp files)
          (mapcar kill-fun files)
        (funcall kill-fun files))))

(setq org-publish-project-alist
      '(
        ("all-notes"
         :components ("book-notes" "notes" "bible-notes" "sbts-notes"))
        ("book-notes"
         :base-directory "~/Documents/notes/book-notes/"
         :publishing-directory "~/Documents/zenodotus-notes/book-notes"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head "<link rel=\"stylesheet\" href=\"../adh-notes.css\" type=\"text/css\"/>"
         :preparation-function (lambda (&rest args)
                                 (adh-kill-if-visiting
                                  "~/Documents/notes/book-notes/sitemap.org"))
         )
        ("notes"
         :base-directory "~/Documents/notes/notes/"
         :publishing-directory "~/Documents/zenodotus-notes/notes"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head "<link rel=\"stylesheet\" href=\"../adh-notes.css\" type=\"text/css\"/>"
         :preparation-function (lambda (&rest args)
                                 (adh-kill-if-visiting
                                  "~/Documents/notes/notes/sitemap.org"))
         )
        ("bible-notes"
         :base-directory "~/Documents/notes/bible-notes/"
         :publishing-directory "~/Documents/zenodotus-notes/bible-notes"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head "<link rel=\"stylesheet\" href=\"../adh-notes.css\" type=\"text/css\"/>"
         :preparation-function (lambda (&rest args)
                                 (adh-kill-if-visiting
                                  "~/Documents/notes/bible-notes/sitemap.org"))
         )
        ("sbts-notes"
         :base-directory "~/Documents/notes/sbts-notes/"
         :publishing-directory "~/Documents/zenodotus-notes/sbts-notes"
         :recursive t
         :publishing-function org-html-publish-to-html
         :auto-sitemap t
         :html-head "<link rel=\"stylesheet\" href=\"../adh-notes.css\" type=\"text/css\"/>"
         :preparation-function (lambda (&rest args)
                                 (adh-kill-if-visiting
                                  "~/Documents/notes/sbts-notes/sitemap.org")))))

;; Export org files to .docx files with nice formatting by selecting template
(setq org-odt-preferred-output-format "docx")

(setq org-odt-convert-processes '(("LibreOffice"
                                  "/Applications/LibreOffice.app/Contents/MacOS/soffice \
--headless --convert-to %f%x --outdir %d %i") 
                                  ("unoconv -f %f -o %d %i")))

;; use html checkboxes when exporting [ ] or [X] to html
(setq org-html-checkbox-type 'html)
)

(with-timer "custom org links"
;; languages markup for export to LaTeX
;; can add some more parameters for link, including styling the face.
(defface org-lang-link '((t (:inherit org-drawer :unerline nil)))
  "Face for org-lang-links")

(org-link-set-parameters
 "lang"
 :follow nil
 :export (lambda (path desc format)
           (cond
            ((eq format 'html)
             (format "<span class=\"%s\">%s</span>" path desc))
            ((eq format 'latex)
             (format "\\text%s{%s}" path desc))))
 :face 'org-lang-link)

;; quotation markup for export to LaTeX
;; Need to find an equivalent system for html as csquotes provides for LaTeX, of
;; using inline format if quote is short and block quote style if quote is
;; longer.
;; helper functions for org quote links
(defun adh-oer-get-author (bib-key)
  "Get the author or editors of the work referenced by `bib-key'
in a useful format for exporting references in org documents"
(let* ((author-string (bibtex-completion-get-value "author"
                                                   (bibtex-completion-get-entry
                                                    bib-key)))
       (editor-flag (if author-string
                         nil
                         (setq author-string (bibtex-completion-get-value
                                                     "editor"
                                                     (bibtex-completion-get-entry
                                                      bib-key)))
                         t))
       (author-list (mapcar (lambda (author-name)
                              (if (s-match ", " author-name)
                                  (nth 0 (s-split "," author-name))
                                (car (last (s-split " " author-name)))))
                            (s-split " and " author-string))))
  (format "%s%s"
          (cond
           ((eq (length author-list) 1) (car author-list))
           ((eq (length author-list) 2) (s-join " and " author-list))
           ((eq (length author-list) 3) (format "%s, %s and %s" (nth 0 author-list)
                                                (nth 1 author-list)
                                                (nth 2 author-list)))
           (t (format "%s et al." (car author-list))))
          (if editor-flag
              " (ed)"
            ""))))

(defun adh-oer-get-title (bib-key)
  "Get the title of the work referenced by `bib-key'. If there's a short title,
get that, otherwise the full title."
  (let ((title (bibtex-completion-get-value "shorttitle"
                                            (bibtex-completion-get-entry
                                             bib-key))))
    (if title
        title
      (bibtex-completion-get-value "title" (bibtex-completion-get-entry
                                            bib-key)))))

;; set up quote org link
(defface org-quote-link '((t (:inherit org-date :underline nil)))
  "Face for org-quote-links.")

(org-link-set-parameters
 "quote"
 :follow nil
 :export (lambda (path desc format)
           (cond
            ((eq format 'html)
             ;; Should reference be handled differently if the file which
             ;; contains the quote is the main note file for that source, or if
             ;; it a reference to the source/quote from elsewhere? Currently
             ;; isn't, will see if it gets annoying enough to be worth changing.
             ;; May also be worth making the reference into a link? Or having a
             ;; footnote that contains a fuller reference?
             (format "<blockquote cite=\"%s\">%s (%s, <i>%s</i>%s)</blockquote>"
                     ;; bibtex key
                     (nth 1 (s-split ":" path))
                     ;; quote text
                     (adh-safe-brace-to-sqbr-html desc)
                     ;; author
                     (org-html-convert-special-strings (adh-oer-get-author
                                                        (nth 1 (s-split ":" path))))
                     ;; title
                     (org-html-convert-special-strings (adh-oer-get-title
                                                        (nth 1 (s-split ":" path))))
                     ;; page number
                     (if (>= (length (s-split ":" path)) 3)
                         (concat ", p. " (car (last (s-split ":" path))))
                       "")))
            ((eq format 'latex)
             (format "\\blockcquote[%s][%s]{%s}{%s}"
                     ;; if 4 elements to link path, there is a pre-note, so use
                     ;; this, else return blank string
                     (if (>= (length (s-split ":" path)) 4)
                         (nth 2 (s-split ":" path))
                       (concat))
                     ;; if 3 or 4 elements to link path, there is a post-note
                     ;; (likely page number) so use post note
                     (if (>= (length (s-split ":" path)) 3)
                         (car (last (s-split ":" path)))
                       (concat))
                     ;; this should always be the bib key
                     (nth 1 (s-split ":" path))
                     ;; description, which in this case is the actual text of
                     ;; the quote
                     (adh-safe-brace-to-sqbr desc)))))
 :face 'org-quote-link
 :display 'org-link)

;; Set up insertion of quotes along with citation for use in org-link format
;; defined above.
;; Uses helm-bibtex to get citation and opens a temporary buffer for editing the
;; quote itself, to make full use of text editing features.
;;
;; Alternatively, a new helm-bibtex action could be added which returns the key
;; instead of inserting it, but that would change the default behaviour of
;; helm-bibtex. It will be worth doing if I only insert references through other
;; interfaces, but not if I use these. Perhaps a better idea is to add a

(defun adh-get-bibtex-key-from-helm ()
  "Use helm-bibtex to select a source and return the source
citation as a string.

A normal call to helm-bibtex will insert the citation to the
buffer, and because the insertion code is hijacked by org-ref,
this work around is necessary. It isn't very neat, but it makes
use of helm-bibtex, and is better than starting from scratch to
do that."
  (interactive)
  (progn (let ((org-ref-prefer-bracket-links nil)
               (ref-string "")
               (bibtex-ref-buffer "temp-bibtex")
               (orig-buffer (current-buffer))
               (orig-buffer-file (buffer-file-name)))
           (switch-to-buffer (generate-new-buffer bibtex-ref-buffer))
           (org-mode)
           (helm-bibtex nil nil
                        (if (and orig-buffer-file
                                 (equal (file-name-directory orig-buffer-file)
                                        (concat bibtex-completion-notes-path "/")))
                            (file-name-base orig-buffer-file)
                          ""))
           (setq ref-string (buffer-string))
           (switch-to-buffer orig-buffer)
           (kill-buffer bibtex-ref-buffer)
           ref-string)))

(defun adh-insert-org-quote-link ()
  "Create an org quote link by prompting for components, then
insert the formatted quote link into buffer.

Makes use of helm-bibtex to get citation, and adh-edit-quote to get the quote
text."
  (interactive)
  (let ((reference-string (adh-get-bibtex-key-from-helm))
        (post-note-string (read-string "Page number: " nil nil nil nil))
        (description-string (adh-edit-quote "")))
    (insert (org-link-make-string (concat "quote:" reference-string ":"
                                          post-note-string)
                                  description-string))))

;; autosave org buffers after common edits that don't autosave
(add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
(add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)

;; When displaying agenda, show it full screen. When quitting, restore previous
;; window layout
(setq org-agenda-window-setup 'only-window
      org-agenda-restore-windows-after-quit t)

;; Keep links in stored list after insterting (useful for putting links in more
;; than one place or using them multiple times). Search is good, so it's no
;; major disadvantage to pile up the links on the stack.
(setq org-link-keep-stored-after-insertion t)

;; don't allow editing of hidden areas in org files.
;; If edit is attempted, deal with it smartly
;; NOTE: this may be better set to show-and-error (shows the region and throws
;; an error to abort the edit), but see how it does for now
(setq org-catch-invisible-edits 'show-and-error)

;; Edit text of quotation links in a separate buffer.
;; Ordinarily editing text in a link description doesn't use most of the text
;; editing features of a normal org mode section, e.g. spell check. It is also
;; difficult to edit because the full line isn't visible, jumping around text
;; may result in leaving the link description, etc.
(defun adh-edit-quote (the-quote)
  "Edit a quote in a buffer, which can then be put in an org quote link."
  (interactive)
  (let ((this-buffer (buffer-name))
        (new-quote the-quote)
        (quote-buffer "*quote-text*"))
    (save-excursion
      (save-window-excursion
        (switch-to-buffer-other-window quote-buffer)
        (set-buffer quote-buffer)
        (org-mode)
        (adh-edit-quote-mode)
        (if (stringp the-quote) (insert the-quote))
        (unwind-protect
            (recursive-edit)
          (if (get-buffer-window quote-buffer)
              (progn
                (setq new-quote (buffer-substring (point-min) (point-max)))
                (kill-buffer quote-buffer))))
        (switch-to-buffer this-buffer)
        (adh-sqbr-to-safe-brace new-quote)))))

(defvar adh-edit-quote-mode-map (make-sparse-keymap)
  "Keymap used for `adh-edit-quote-mode', a minor mode.
Use this map to set additional keybindings for when using a quote buffer.")

(define-minor-mode adh-edit-quote-mode
  "Minor mode for special key bindings in a quote editing buffer."
  nil "quote" adh-edit-quote-mode-map
  (setq-local header-line-format (substitute-command-keys
                                  "\\<adh-edit-quote-mode-map>Quote buffer. \
Finish `\\[adh-edit-quote-finalize]'."))
  (message (substitute-command-keys "\\<adh-edit-quote-mode-map>\
When you're done editing press `\\[adh-edit-quote-finalize]' to continue.")))

(defun adh-edit-quote-finalize ()
    "Complete the quote edit."
  (interactive)
  (exit-recursive-edit))

(define-key adh-edit-quote-mode-map "\C-c\C-c" 'adh-edit-quote-finalize)

;; some functions for handling conversion from the actual quote text to
;; org-link-description safe text
(defun adh-sqbr-to-safe-brace (str)
    "Format string str into quote-link safe text (org mode link description).

In quote-link data, can't use square brackets [ and ] in link
description, so replace them with the safe strings, @< and @>
respectively. Then, when exporting, replace them for normal usage
using `adh-safe-brace-to-sqbr'."
  (replace-regexp-in-string "\\[" "@<"
                            (replace-regexp-in-string "\\]" "@>" str)))

(defun adh-safe-brace-to-sqbr (str)
    "Format `str' from quote-link safe text (org mode link
description) into normal formatting for exported file use.

In quote-link data, can't use square brackets [ and ] in link
description, so replace them with the safe strings, @< and @>
respectively, using `adh-sqbr-to-safe-brace'. Then, when
exporting, replace them for normal usage."
  (replace-regexp-in-string (regexp-quote "@<") "["
                            (replace-regexp-in-string (regexp-quote "@>") "]" str)))

(defun adh-safe-brace-to-sqbr-html (str)
    "Format `str' from quote-link safe text (org mode link
description) into normal formatting for exported file use.

In quote-link data, can't use square brackets [ and ] in link
description, so replace them with the safe strings, @< and @>
respectively, using `adh-sqbr-to-safe-brace'. Then, when
exporting, replace them for normal usage."
  (replace-regexp-in-string (regexp-quote "@&lt;") "["
                            (replace-regexp-in-string (regexp-quote "@&gt;") "]" str)))
)

(with-timer "org special keys"
;; in org headlines, jump to start and end of headline text with a single C-a or
;; C-e. To get to the true start of line and end of line (before stars and todo
;; status, or after tags), simply press again.
(setq org-special-ctrl-a/e t)
;; todo: set other org-special values, and fix this to work with visual fill (https://www.reddit.com/r/emacs/comments/fjku3d/visual_line_mode_conflicts_with_orgspecialctrlae/)
)

(with-timer "habits"
;; org-habit view in agenda
(add-to-list 'org-modules 'habit)
; (require 'org-habit)
(autoload 'org-agenda "org-habit" "" t)

(eval-after-load "org-habit"
  '(setq org-habit-show-all-today t
         org-habit-show-done-always-green t
         org-habit-graph-column 60))
)

(with-timer "gui, extra, lists, and bullets"
;; Allow hiding of org headlines while including any content under them.
;; Particularly useful for including extra structure and TODOs in document.
;; (require 'ox-extra)
;; (ox-extras-activate '(ignore-headlines))
(autoload 'org-mode "ox-extra" "" t)
(eval-after-load "ox-extra" '(ox-extras-activate '(ignore-headlines)))

;; org bullets set up
; (require 'org-bullets)
(defun adh-org-bullet-enable ()
  "Enable org bullets (function to be called by `org-mode-hook')"
  (require 'org-bullets)
  (org-bullets-mode 1))

(add-hook 'org-mode-hook #'adh-org-bullet-enable)

;; use brackets around org-ref links, even when no page reference added
(setq org-ref-prefer-bracket-links t)

;; allow use of letters as well as numbers to define lists
(setq org-list-allow-alphabetical t)

;; set up newline handling in org mode.
;; Main text should use autofill to hard line-break and wrap at `fill-column'
;; width.
;; Org headlines should not hard break (new lines not allowed in headings), so
;; should instead soft wrap. This should be done at the window edge, not
;; fill-column (this stops a fight between auto-fill and visual-fill), but
;; should break lines between words, not in the middle of a word.
(defun adh-org-hard-linebreak-setup ()
    "Set up line breaking as I like it in Org mode.

- Normal text should be hard wrapped at fill-column (usually 80 characters)
- Headlines should wrap at window edge
- Headlines should wrap between words, not in the middle of a word"
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode t)
  (visual-line-mode t)
  (toggle-word-wrap t)
  (visual-fill-column-mode 0))

(add-hook 'org-mode-hook #'adh-org-hard-linebreak-setup)

(defun adh-soft-linebreak-setup ()
  "Set up line breaking for soft linebreaks.

Normal text should *not* be hard wrapped, as in `adh-org-linebreak-setup'.
Though I usually prefer hard-breaks, when copying text to another application,
often one-line-per-paragraph works better for copy and pasting, so this set up
gives that option. Instead, `visual-line-mode' is used to provide soft
line-breaks."
  (interactive)
  (set-fill-column 80)
  (auto-fill-mode 0)
  (visual-line-mode t)
  (toggle-word-wrap t)
  (visual-fill-column-mode t))

;; enable org-indent-mode
(defun adh-org-indent-enable ()
  "Enable org-indent-mode (function to be called by `org-mode-hook')"
  (org-indent-mode 1)
  (diminish 'org-indent-mode)
  (diminish 'flyspell-mode))

(add-hook 'org-mode-hook #'adh-org-indent-enable)

;; set up non-breaking spaces for org export
(setq org-entities-user
      '(("nbsp" "~" nil "&nbsp;" " " " " " ")))

;; hide emphasis markers (/ and * etc) when viewing org files
(setq org-hide-emphasis-markers t)
)

(with-timer "org-id"
;; Use ID property for org links if it exists. But don't create if it doesn't
;; exist.
;; 2020-04-06: I think this is the behaviour I want, always use ID if possible,
;; but some org files or headings may be best without IDs, so don't force
;; creation of an ID if creating a link to them.
(require 'org-id)
(setq org-id-link-to-org-use-id t)
)

(with-timer "org-zett"
(require 'hydra)
; (require 'org-zett)
(autoload 'org-zett-add-note-link "org-zett")
(autoload 'org-zett-add-inline-link "org-zett")

(defhydra hydra-org-zett (:color red
                          :hint nil)
  "
^Link notes^           ^Util^
^^^^^^^^--------------------------------------
_z_: Zett link         _r_: Refresh cache
_i_: Inline link
"
  ("z" org-zett-add-note-link)
  ("i" org-zett-add-inline-link)
  ("r" (lambda ()
         (interactive)
         (org-refile-cache-clear))))

(define-key org-mode-map (kbd "C-c z") 'hydra-org-zett/body)
)

(with-timer "protect and ispell"
;; do not kill hidden org headlines, at least not without asking nicely
(setq org-ctrl-k-protect-subtree t)

;; don't check spelling of certain parts of org files.
;; Property settings, code blocks, etc should not be spell checked by
;; ispell/aspell
;; Adapted from https://endlessparentheses.com/ispell-and-org-mode.html
(defun adh-org-ispell-setup ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '(org-property-re forward-line))
  ; property lines
  (add-to-list 'ispell-skip-region-alist '("^#\\+[A-Za-z_]*:" "$"))
  ; note-link drawers
  (add-to-list 'ispell-skip-region-alist '("^:NOTE-LINKS:$" "^:END:$"))
  ; check link description, but link text
  (add-to-list 'ispell-skip-region-alist '("\\[\\[" "\\]\\["))
  ;; (add-to-list 'ispell-skip-region-alist '("~" "~"))
  ;; (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'adh-org-ispell-setup)

; I never use C-' to cycle through agenda files, and I keep hitting it by
; accident, especially when trying to change input ( C-\ ), so disable command
; (global-unset-key (kbd "C-'"))
(defun adh-org-unbind-cycle-agenda-files ()
  "Function to be called in org-mode-hook to unbind C-' from `org-cycle-agenda-files'."
  (define-key org-mode-map (kbd "C-'") nil))
(add-hook 'org-mode-hook #'adh-org-unbind-cycle-agenda-files)

)

(provide 'adh_org)
;;; adh_org.el ends here
