;; setup how emacs does things with files and buffers etc

;; autosave setup - stop cluttering up directories with autosaves
;; (fix) from whattheemacsd.com
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
"Kill all buffers but the current one. Don't mess with special
buffers."
(interactive)
(dolist (buffer (buffer-list))
  (unless (or (eql buffer (current-buffer)) (not (buffer-file-name
						  buffer)))
    (kill-buffer buffer))))


;; function to copy the name of the file being viewed to the
;; clipboard.
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
  "Open up init.el fine and init/ directory for editing"
  (interactive)
  (delete-other-windows)
  (find-file "~/.emacs.d/init.el")
  (split-window)
  (other-window 1)
  (dired "~/.emacs.d/init/."))

;; bind to C-c e i
(global-set-key (kbd "C-c e i") 'adh-edit-init)


;; setup new init file and include in load
;; todo: make work!
(defun adh-new-init-file ()
  "Add a new file to init loading with given name."
  (interactive)
  (set new-init-file-name (read-string "File name: " ))
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
  (find-file "~/Dropbox/Notes/Redemptive_historical_framework.org")
  (set-fill-column 95)
  (find-file "~/Dropbox/Notes/Numbers.org")
  (set-fill-column 95)
  (split-window-right)
  (other-window 1)
  (find-file "~/Dropbox/Notes/2Corinthians.org")
  (set-fill-column 95)
  (other-window 1))

(provide 'adh_buffersandfiles)
