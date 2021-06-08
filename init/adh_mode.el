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
;; (add-hook 'before-save-hook 'whitespace-cleanup)

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

;; try not using full-frame for helm-bibtex
(setq helm-bibtex-full-frame nil)

;; add bib files to helm-bibtex search path
(setq bibtex-completion-bibliography '("~/Projects/WritingTools/Lit.bib"
                                       "~/Projects/WritingTools/PopSciHist.bib"
                                       "~/Projects/WritingTools/Theology.bib"))

;; change default action of helm-bibtex to insert citation
(require 'helm-bibtex)
(helm-delete-action-from-source "Insert citation" helm-source-bibtex)
(helm-add-action-to-source "Insert citation"
                           'helm-bibtex-insert-citation helm-source-bibtex 0)

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
(setq bibtex-completion-additional-search-fields '(subtitle))

;; open PDF files outside of emacs in standard programme
(setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

;; add subtitle field to helm-bibtex display
(setq bibtex-completion-display-formats
      '((t . "${author:15} ${title:*} ${subtitle:25} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))

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
#+STARTUP: overview lognoterefile
#+LATEX_HEADER: \\addbibresource{/Users/adh/Projects/WritingTools/Theology.bib}
#+LATEX_HEADER: \\addbibresource{/Users/adh/Projects/WritingTools/PopSciHist.bib}
#+CREATED_DATE: ")

;; set helm-bibtex key binding
(global-set-key (kbd "C-c m b") 'helm-bibtex)

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
            (auto-fill-mode 1)
            (show-paren-mode 1)
            (flyspell-prog-mode)))

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
(setq sql-product 'mysql)

;; set up for python interpreter
(setq python-shell-interpreter "python3")

(provide 'adh_mode)
;;; adh_mode.el ends here
