(provide 'adh_org)

;; ensure files with .org extension use org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; have *todo* buffer use org mode
;(add-to-list 'auto-mode-alist '("*todo*" . org-mode))

;; add files to org agenda
(setq org-agenda-files (quote ("~/Dropbox/Org_files/todo.org"
			       "~/Dropbox/Org_files/hpcomm.org"))) 

;; set diary for inclusion in agenda
(setq org-agenda-include-diary t)

;; TODO keyword states for org files
(setq org-todo-keywords
  '((sequence "TODO" "BEGUN" "|" "DONE")))

;; global keybindings for org mode
(eval-after-load "org"
  '(progn
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-cc" 'org-capture)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)))
