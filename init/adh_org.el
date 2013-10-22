(require 'org)

;; ensure files with .org extension use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; have *todo* buffer use org mode
;(add-to-list 'auto-mode-alist '("*todo*" . org-mode))

(defvar adh-dropbox-location
  (cond ((eq system-type 'gnu/linux) "~/Dropbox/")
        ((eq system-type 'windows-nt) "d:/Dropbox/")
        (t "~/Dropbox/"))
  "Holds the location of the Dropbox root directory for the current system")

;; add files to org agenda
(setq org-agenda-files
      (list (concat adh-dropbox-location "Org_files/todo.org")
             (concat adh-dropbox-location "Org_files/hpcomm.org")
             (concat adh-dropbox-location "Org_files/Work.org")
             (concat adh-dropbox-location
                     "Org_files/CICCU_Treasurer.org")
             (concat adh-dropbox-location
                     "Org_files/CICCU_Exec.org")
             (concat adh-dropbox-location "Org_files/shopping.org")
             (concat adh-dropbox-location "Org_files/computer_stuff.org")))

;; set diary for inclusion in agenda
(setq org-agenda-include-diary t)

;; TODO keyword states for org files
(setq org-todo-keywords
  '((sequence "TODO" "|" "DONE")))

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
  (interactive)
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (adh-org-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (adh-org-update-parent-cookie))

;; setup capture target file
(setq org-default-notes-file "~/Dropbox/Org_files/todo.org")

;; setup capture templates
(setq org-capture-templates
  '(("t" "Templates for TODO items")
    ("tt" "Todo" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                      "Tasks")
     "* TODO %?\n %i\n"
     :kill-buffer)
    ("tr" "Treasurer" entry (file
                             "~/Dropbox/Org_files/CICCU_Treasurer.org")
     "* TODO %?\n %i\n"
     :kill-buffer)
    ("te" "Exec" entry (file "~/Dropbox/Org_files/CICCU_Exec.org")
     "* TODO %?\n %i\n"
     :kill-buffer)
    ("tc" "Computer" entry (file+headline
                            "~/Dropbox/Org_files/computer_stuff.org"
                            "Unfiled")
     "* TODO %?\n %i\n"
     :kill-buffer)
    ("tw" "Work" entry (file "~/Dropbox/Org_files/Work.org")
     "* TODO %?\n %i\n"
     :kill-buffer)
   ("n" "Note" entry (file+headline "~/Dropbox/Org_files/todo.org"
                                    "Notes")
        "* %?\n %i\n %a")
   ("l" "Link" entry (file+headline "~/Dropbox/Org_files/todo.org"
                               "Web Links")
    "* %a\n %?\n %i")))

;; start org protocol - for creating links etc to external
;; applications
;(require 'org-protocol)

;(setq org-protocol-default-template-key "l")

;; set pdf application for opening links
;; from stack overflow
(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")

;; setup refiling to up to 3rd level headings
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))

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
(setq org-mobile-directory (concat adh-dropbox-location "MobileOrg"))
(setq org-mobile-inbox-for-pull (concat adh-dropbox-location "Org_files/todo.org"))

(provide 'adh_org)
