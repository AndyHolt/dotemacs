;;; adh_buffersandfiles.el --- Set up how emacs deals with files and buffers -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:33
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Various changes and customisations for handling of files and buffers.

;;; Code:

;; autosave setup
;; stop cluttering up directories with autosaves
;; from whattheemacsd.com
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; make backups of files, even when they're in version control
;; from whattheemacs.com
(setq vc-make-backup-files t)

;; function to kill all buffers except current one and special ones
;; taken from "prelude" emacs setup
(defun adh-kill-other-buffers ()
  "Kill all buffers but the current one.  Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name
                                                    buffer)))
      (kill-buffer buffer))))

;; function to copy the name of the file being viewed to the clipboard.
;; taken from "prelude" emacs setup
(defun adh-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard."
               filename))))

;; bind function to C-c M-w
(global-set-key (kbd "C-c M-w") 'adh-copy-file-name-to-clipboard)

;; rename current buffer and the file its visiting.
;; from https://github.com/bbatsov/prelude/
(defun adh-rename-buffer-and-file ()
  "Renames current buffer and file its visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!"
                        new-name))
              (t
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

;; open up files ready to edit init files
(defun adh-edit-init ()
  "Open up init.el fine and init/ directory for editing."
  (interactive)
  (delete-other-windows)
  (find-file "~/.emacs.d/init.el")
  (split-window)
  (other-window 1)
  (dired "~/.emacs.d/init/."))

;; bind to C-c e i
(global-set-key (kbd "C-c e i") 'adh-edit-init)

;; setup new init file and include in load
;; [todo] - make new init file function work!
(defun adh-new-init-file ()
  "Add a new file to init loading with given name."
  (interactive)
  (set 'new-init-file-name (read-string "File name: " ))
  (delete-other-windows)
  (find-file "~/.emacs.d/init.el")
  (goto-char (point-max))
  (search-backward "require")
  (end-of-line)
  (newline-and-indent)
  (insert (format "(require '%s)" new-init-file-name))
  (split-window-right)
  (find-file (format "~/.emacs.d/init/%s.el" new-init-file-name))
  (insert (format "(provide '%s)" new-init-file-name))
  (newline 2))

;; remeber position of point between sessions:
;; from https://github.com/magnars/.emacs.d/
;; (author of emacs rocks)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places"
                                        user-emacs-directory))

(defun adh-qt-notes ()
  "Open up notes of quiet times."
  (interactive)
  (delete-other-windows)
  (find-file "~/Documents/notes/notes/Redemptive_historical_framework.org")
  (set-fill-column 95)
  (find-file "~/Documents/notes/notes/Judges.org")
  (set-fill-column 95)
  (split-window-right)
  (other-window 1)
  (find-file "~/Documents/notes/notes/Mark.org")
  (set-fill-column 95)
  (other-window 1))

;; setup uniquify - if multiple buffers are visiting different files with same
;; name, rename the buffers with the distinctive parts of the directory tree.
(require 'uniquify)
;; separate distinctive directory name from buffer name with forward slash
(setq uniquify-buffer-name-style 'forward)
;; only display the different part of the directory tree, not the whole thing
(setq uniquify-strip-common-suffix t)

;; ibuffer configuration
;; group buffers by vc parent directory
(add-hook 'ibuffer-hook
  (lambda ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

;; include vc status in ibuffer list
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(defun adh-find-file-copy-path (&optional initial-input)
    "Use interactive file search to find a file, but instead of finding
(visiting/opening) the file, copy the full path to the clipboard."
  (interactive)
  (counsel--find-file-1
   "Copy path: " initial-input
   'kill-new
   'adh-find-file-copy-path))

(defun adh-find-file-insert-path (&optional initial-input)
    "Use interactive file search to find a file, but instead of finding
(visiting/opening) the file, insert the path at point."
  (interactive)
  (counsel--find-file-1
   "Insert path: " initial-input
   'insert
   'adh-find-file-copy-path))

;; dedicate-windows-manually
;; taken from https://www.emacswiki.org/emacs/dedicate-windows-manually.el
(require 'dedicate-windows-manually)

;; enable recursive minibuffer and indicate depth
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(provide 'adh_buffersandfiles)
;;; adh_buffersandfiles.el ends here
