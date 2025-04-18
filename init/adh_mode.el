;;; adh_mode.el --- Set up modes -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Thu 17 Apr 2014 00:26
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Set up associations and settings for various major and minor modes.
;; In-mode settings are done in individual mode files, this file handles the
;; modes themselves.
;; Setup mode hooks and file-mode associations.

;;; Code:

;; [fix] - don't run whitespace-cleanup on auto-save, only manual save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; matlab major mode when loading file with .m extension
(setq auto-mode-alist
      (cons'("\\.m$" . matlab-mode) auto-mode-alist))

;; set matlab indent level
(setq matlab-indent-level 2)
(setq matlab-cont-level 2)

;; use shell script mode for zshrc files
(setq auto-mode-alist
      (cons'("zshrc" . shell-script-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons'("zsh_aliases" . shell-script-mode) auto-mode-alist))

;; use emacs lisp mode for Cask files
(setq auto-mode-alist
      (cons'("Cask" . emacs-lisp-mode) auto-mode-alist))

;; use conf-mode for watson and terminator config files
(setq auto-mode-alist
      (cons'(".watsonrc" . conf-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons'("terminator_config" . conf-mode) auto-mode-alist))

;; ruby mode for Gemfiles and Guardfiles
(setq auto-mode-alist
      (cons'("Gemfile" . ruby-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons'("Guardfile" . ruby-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))

;; PDFs shouldn't be opened in Emacs. Load in hex mode as it's much faster.
;; [todo] - change to auto-open PDFs in standard program instead?
; (add-to-list 'auto-mode-alist '("\\.pdf\\'" . hexl-mode))

;; use markdown mode for .md files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; citations from bibtex in markdown mode
(defun adh-markdown-citation-config ()
  "Set up helm-bibtex for markdown mode."
    (local-set-key (kbd "C-c (") 'helm-bibtex))

(add-hook 'markdown-mode-hook 'adh-markdown-citation-config)

;; bibtex mode configuration
;; Set to use biblatex by default
(setq bibtex-dialect "biblatex")

;; add review entry type for bibtex mode
(setq adh-bibtex-review-entry
  '("Review" "Book Review in Journal"
    (("author")
     ("title" "Title of review article"))
    (("journaltitle")
     ("date"))
    (("shortjournal" "Short version or acronym of journal title")
     ("volume" "Volume of the journal")
     ("number" "Number of the journal (only allowed if entry contains volume)")
     ("pages" "Pages in the journal")
     ("url")
     ("urldate" "Date accessed")
     ("doi")
     ("eprinttype" "Eprint resource type, e.g. jstor or googlebooks")
     ("eprint" "Eprint identifier (must also use eprinttype)")
     ("note")
     ("file"))))

(add-to-list 'bibtex-biblatex-entry-alist adh-bibtex-review-entry t)
(add-to-list 'bibtex-BibTeX-entry-alist adh-bibtex-review-entry t)

;; try not using full-frame for helm-bibtex
(setq helm-bibtex-full-frame nil)

;; add bib files to helm-bibtex search path
(setq bibtex-completion-bibliography '("~/Projects/WritingTools/Lit.bib"
                                       "~/Projects/WritingTools/PopSciHist.bib"
                                       "~/Projects/WritingTools/Engineering.bib"
                                       "~/Projects/WritingTools/Theology.bib"))

;; change default action of helm-bibtex to insert citation
(autoload 'helm-bibtex "helm-bibtex" "" t)

(eval-after-load "helm-bibtex"
  '(progn
     (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
     (helm-add-action-to-source "Insert citation"
                                'helm-bibtex-insert-citation helm-source-bibtex
                                0)))

;; TODO: get this working! Currently not being added as source, as function does
;; not work when added as source!
;; TODO: Then, after getting the basic version running, add a fallback function
;; version which will open a url or doi if ;; there is no pdf file (see
;; `bibtex-completion-open-any')
;;
;; helm-bibtex action for visiting directory in dired if PDF not available to
;; open (i.e. when icloud has removed file from computer)
(defun adh-bibtex-completion-open-pdf-icloud (keys)
  "Open the PDFs associated with the marked entries using the function specified
in `bibtex-completion-pdf-open-function'.
If the designated file cannot be found (not currently downloaded, but stored on
  icloud), go the directory, where it can be downloaded and then opened manually."
  (dolist (key keys)
    (let* ((pdf (bibtex-completion-get-value bibtex-completion-pdf-field
                                            (bibtex-completion-get-entry-1
                                             key)))
           (pdfd (file-name-directory pdf)))
      (cond
       ((file-exists-p pdf)
        (funcall bibtex-completion-pdf-open-function pdf))
       ((file-exists-p pdfd)
        (dired pdfd))
       (t
        (message "No PDF or directory found for entry %s" key))))))

;; (helm-add-action-to-source "Open PDF or directory"
;;                            'adh-bibtex-completion-open-pdf-icloud
;;                            helm-source-bibtex
;;                            1)
; (helm-delete-action-from-source "Open PDF or directory" helm-source-bibtex)


;; reverse order of helm candidates
;; By default, orders candidates from bottom of bib file to top, giving "most
;; recent" candidates at top. However, this only makes sense if bib file has
;; entries appended, not sorted into order.
(advice-add 'bibtex-completion-candidates
            :filter-return 'reverse)

;; use preview to open PDFs
;; [todo] - not working! Needs some fixing
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (call-process "open" nil 0 nil "-a" "/Applications/Preview.app" fpath)))

;; setup of helm-bibtex for pdf file links
;; set a bibtex field for path to file
(setq bibtex-completion-pdf-field "File")
;; backup: a directory location
;; (files here should be stored with name: bibtex-key.pdf)
(setq bibtex-completion-library-path '("/Users/adh/Documents/BooksAndArticles"
                                       "/Users/adh/Documents/Manuels"))

;; setup of helm-bibtex for notes (linked to bibkey and PDF file)
(setq bibtex-completion-notes-path "/Users/adh/Documents/notes/book-notes")

;; add additional fields to bibtex search in helm matching
(setq bibtex-completion-additional-search-fields
      '(subtitle shortseries series booktitle shortbooktitle shortjournal
                 maintitle mainsubtitle))

;; open PDF files outside of emacs in standard programme
(setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

;; add subtitle field to helm-bibtex display
(setq bibtex-completion-display-formats
      '((book . "${author:15} ${title:*} ${subtitle:15} ${shortseries:5} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        (collection . "${author:15} ${title:*} ${subtitle:15} ${shortseries:5} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        (incollection . "${author:15} ${title:*} ${subtitle:10} in ${shortbooktitle:6} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        (inbook . "${author:15} ${title:*} ${subtitle:10} in ${booktitle:15} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        (article . "${author:15} ${title:*} ${subtitle:10} ${shortjournal:6} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        (t . "${author:15} ${title:*} ${subtitle:15} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))

;; [todo] - find a way to add current date to note header
;; add date to notes header when creating a new note file
;; (setq bibtex-completion-notes-template-multiple-files
;;       "#+TITLE: Notes on: ${author-or-editor} (${year}): $title}
;; #+DATE:
;;
;; ")

(setq bibtex-completion-notes-template-multiple-files
      "#+TITLE: Notes on: ${author-or-editor} (${year}): ${title}
#+AUTHOR: Andy Holt
#+SETUPFILE: adh-org-notes-export-setup.org
#+LANGUAGE: en
#+OPTIONS: num:t toc:t
#+STARTUP: overview lognoterefile
#+BIBLIOGRAPHY: /Users/adh/Projects/WritingTools/Theology.bib
#+BIBLIOGRAPHY: /Users/adh/Projects/WritingTools/Engineering.bib
#+BIBLIOGRAPHY: /Users/adh/Projects/WritingTools/PopSciHist.bib
#+BIBLIOGRAPHY: /Users/adh/Projects/WritingTools/Lit.bib
#+CREATED_DATE: %T

* Summary
* Structure
* Key Arguments
* Quotes
* Critical engagement (evaluation, concerns, limits, push back, reflections)
* Citations/references for further research
")

;; Allow setting a current "default" helm-bibtex entry for pre-selection.
;; e.g. when taking notes from a particular book, but not in that book's main
;; note file, have the entry pre-selected by default
(defvar adh-helm-bibtex-default-selection 'nil)

(defun adh-helm-bibtex-set-default-selection ()
  "Select an entry to use as `adh-helm-bibtex-default-selection'."
  (interactive)
  ;; Need to let helm-action be a single action which returns (*not* inserts)
  ;; the bibtex key of a selected entry. Then set
  ;; adh-helm-bibtex-default-selection to that returned key as a string.

)

(defun adh-helm-bibtex-clear-default-selection ()
  "Unset `adh-helm-bibtex-default-selection'."
  (interactive)
  (setq adh-helm-bibtex-default-selection 'nil))

;; set helm-bibtex key binding
(defun adh-helm-bibtex ()
  "Use `helm-bibtex' to select a source. If currently visiting a file in
`bibtex-completion-notes-path' then use that note file's base name as the
default input for `helm-bibtex'. Else just use helm-bibtex as normal."
  (interactive)
  (helm-bibtex nil nil
               (or adh-helm-bibtex-default-selection
                   (if (and buffer-file-name
                            (equal (file-name-directory buffer-file-name)
                                   (concat bibtex-completion-notes-path "/")))
                       (file-name-base buffer-file-name)
                     nil))))


(global-set-key (kbd "C-c m b") 'adh-helm-bibtex)

;; settings for bibtex-clean-entry
(setq bibtex-entry-format '(opts-or-alts required-fields
                                         numerical-fields
                                         page-dashes whitespace
                                         inhereit-booktitle
                                         realign
                                         last-comma
                                         delimiters
                                         sort-fields)
      bibtex-align-at-equal-sign nil
      bibtex-comma-after-last-field nil
      bibtex-field-delimiters 'braces
      bibtex-entry-delimiters 'braces)


;; function to clean up tables from org syntax to md
(defun org2md-table ()
    "Change org-mode table sytnax into markdown format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(defun org2md-table2 ()
    "Change org-mode table sytnax into markdown format."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-|-" nil t) (replace-match "-+-"))
    ))

;; turn on org table minor mode when using markdown mode
(add-hook 'markdown-mode-hook 'orgtbl-mode)
;; convert tables on save
(add-hook 'markdown-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'org2md-table2 nil 'make-it-local)))
;; don't run whitespace cleanup in markdown mode - double spaces at ends of
;; lines are important!
(add-hook 'markdown-mode-hook
         (lambda()
            (remove-hook 'before-save-hook 'whitespace-cleanup 'make-it-local)))

;; auctex mode when loading file with .tex extension.
;; [todo] - Need to setup auctex properly. Not in package repo just now?


(add-hook 'prog-mode-hook
          (lambda ()
            (show-paren-mode 1)
            (flyspell-prog-mode)
            (display-line-numbers-mode t)))

(add-hook 'matlab-mode-hook
          (lambda ()
            ;(abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (auto-fill-mode 1)))

; [todo] - check latex mode settings
; [todo] - use AUCTeX mode? And ensure reftex mode being used.

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (reftex-mode 1)
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (tex-pdf-mode 1)
            (require 'smartparens-latex)))

(require 'whole-line-or-region)
(whole-line-or-region-global-mode t)

(defun adh-text-mode-setup ()
    "Set up text-mode buffers with general settings.

Enable auto-fill-mode for line wrapping and flyspell mode for error correction"
    (auto-fill-mode 1)
    (flyspell-mode 1)
    (diminish 'flyspell-mode))

(add-hook 'text-mode-hook #'adh-text-mode-setup)

;[todo] - consider visual-line-mode for some text modes (e.g. email)
;; (instead of auto-fill-mode)
;; does 'soft' line breaks instead of hard line breaks (text is displayed neatly
;; wrapped, but doesn't actually add newlines into text.)

(add-to-list 'load-path
             "~/.emacs.d/plugins/")

;; (require 'tbemail)

;; (add-hook 'tbemail-mode
;;        (lambda ()
;;          (flyspell-mode 1)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)))

;; add ordinal functionality to format-time-string.
;; This allows specification of dates such as "1st", rather than just "1", using
;; %o.
;; This shouldn't affect other use of format-time-string, as it's just adding
;; %o as a construct, not changing any standard constructs.
;; Code from: https://stackoverflow.com/questions/20316754/how-can-i-add-a-format-specifier-to-format-time-string/20317537#20317537

(defun adh-date-ordinal-construct (n)
  "Special day of month format."
  (format
   (concat
    "%d"
    (if (memq n '(11 12 13)) "th"
      (let ((last-digit (% n 10)))
        (cl-case last-digit
          (1 "st")
          (2 "nd")
          (3 "rd")
          (otherwise "th"))))) n))

(defadvice format-time-string (before adh-date-ordinal-construct activate)
  "Add ordinal to %d."
  (let ((day (nth 3 (decode-time (or time (current-time))))))
    (setq format-string
      (replace-regexp-in-string "%o"
                    (adh-date-ordinal-construct day)
                    format-string))))

;; Select a date via calendar, and insert into buffer at point
(defun adh-insert-date ()
  "Insert a specified date, in chosen format."
  (interactive)
  (let ((selected-date (org-read-date nil t)))
    (insert (completing-read "Date format:"
              (list
               ;; 18/12/18
               (format-time-string "%d/%m/%y" selected-date)
               ;; 18/12/2018
               (format-time-string "%d/%m/%Y" selected-date)
               ;; 18 Dec
               (format-time-string "%-e %b" selected-date)
               ;; 18th Dec
               (format-time-string "%o %b" selected-date)
               ;; 18 Dec 2018
               (format-time-string "%-e %b %Y" selected-date)
               ;; 18th Dec 2018
               (format-time-string "%o %b %Y" selected-date)
               ;; 18 December
               (format-time-string "%-e %B" selected-date)
               ;; 18th December
               (format-time-string "%o %B" selected-date)
               ;; 18 December 2018
               (format-time-string "%-e %B %Y" selected-date)
               ;; 18 December 2018
               (format-time-string "%o %B %Y" selected-date)
               ;; Tue 18 Dec
               (format-time-string "%a %-e %b" selected-date)
               ;; Tue 18th Dec
               (format-time-string "%a %o %b" selected-date)
               ;; Tue 18 Dec 2018
               (format-time-string "%a %-e %b %Y" selected-date)
               ;; Tue 18th Dec 2018
               (format-time-string "%a %o %b %Y" selected-date)
               ;; Tuesday 18 December
               (format-time-string "%A %-e %B" selected-date)
               ;; Tuesday 18th December
               (format-time-string "%A %o %B" selected-date)
               ;; Tuesday 18 December 2018
               (format-time-string "%A %-e %B %Y" selected-date)
               ;; Tuesday 18th December 2018
               (format-time-string "%A %o %B %Y" selected-date)
               ;; 2018-12-18
               (format-time-string "%Y-%m-%d" selected-date)
               ;; org-format
               (format-time-string "%Y-%m-%d %a")
               ;; RFC 2822: Tue, 18 Dec 2018
               (format-time-string "%a, %d %b %Y" selected-date)
)))))

;; Bind adh-insert-date to C-c . in various modes. Can't do this globally, as
;; org-mode uses C-c . for org-time-stamp. But I like the consistency between
;; using C-c . for putting in dates across different modes. And it's a very easy
;; keybinding to use.
(eval-after-load 'bibtex
  '(define-key bibtex-mode-map (kbd "C-c .")
     'adh-insert-date))
(eval-after-load 'mu4e-compose
  '(define-key mu4e-compose-mode-map (kbd "C-c .")
     'adh-insert-date))
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c .")
     'adh-insert-date))
(eval-after-load 'tex-mode
  '(define-key latex-mode-map (kbd "C-c .")
     'adh-insert-date))
(eval-after-load 'text-mode
  '(define-key text-mode-map (kbd "C-c .")
     'adh-insert-date))

;; set up for sql-mode
(setq sql-product 'postgres)

;; set up for python interpreter
(setq python-shell-interpreter "python3")

;; set python indent level
(setq python-indent-offset 4)

;; modes for git configuration files
(autoload 'gitignore-mode "gitignore-mode" "" t)
(add-to-list 'auto-mode-alist '("/.gitignore\\'" . gitignore-mode))

;; use yaml-mode for yaml files. Mainly using yaml files for config files (e.g.
;; tmuxinator)
(autoload 'yaml-mode "yaml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; use go-ts-mode for Go files
(autoload 'go-ts-mode "go-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(provide 'adh_mode)
;;; adh_mode.el ends here
