;;; adh_mode.el --- Set up modes

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

;; add bib files to helm-bibtex search path
(setq bibtex-completion-bibliography '("~/Projects/WritingTools/Theology.bib"))

;; change default action of helm-bibtex to insert citation
(require 'helm-bibtex)
(helm-delete-action-from-source "Insert citation" helm-source-bibtex)
(helm-add-action-to-source "Insert citation"
                           'helm-bibtex-insert-citation helm-source-bibtex 0)

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
(setq bibtex-completion-notes-path "/Users/adh/Documents/BookNotes")

;; add additional fields to bibtex search in helm matching
(setq bibtex-completion-additional-search-fields '(subtitle))

;; add subtitle field to helm-bibtex display
(setq bibtex-completion-display-formats
      '((t . "${author:15} ${title:*} ${subtitle:25} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))

;; set helm-bibtex key binding
(global-set-key (kbd "C-c m b") 'helm-bibtex)

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

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (org-indent-mode 1)))

(require 'whole-line-or-region)
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (whole-line-or-region-mode t)))

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

(provide 'adh_mode)

;;; adh_mode.el ends here
