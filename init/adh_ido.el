;;; adh_ido.el --- Setup for ido mode -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:48
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; ido mode: interactively do things
;;
;; Powerful system for finding things in minibuffer.

;;; Code:

;; use ido mode for easy file and buffer finding
(ido-mode t)

;; don't change working directory when creating new files
(setq ido-auto-merge-work-directories-length -1)

;; some basic config stuff - taked from Thomas Kjeldahl Nilsson
(setq ido-enable-prefix               nil
      ido-enable-flex-matching        t
      ido-use-filename-at-point      'guess
      ido-max-prospects               10
      ido-use-faces                   t
      ido-max-window-height           nil
      ido-case-fold                   t
      ido-confirm-unique-completion   t
      ido-create-new-buffer          'always
      ido-default-file-method        'raise-frame
      ido-default-buffer-method      'raise-frame)

;; ignore some buffers
(setq ido-ignore-buffers '(".*Completions\*"
                           "^\*trace"
                           "^\*compilation"
                           "\*Minibuf-\*"
                           ".*Echo\*"
                           ".*code-conversion-work.*"))

;; try out flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; flx-ido looks better with vertical mode
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; use ido almost everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; jump to home directory when using ido
;;   from whattheemacsd.com
(add-hook 'ido-setup-hook
  (lambda ()
    ;; Go straight home
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
        (interactive)
        (if(looking-back "/")
            (insert "~/")
          (call-interactively 'self-insert-command))))))

(defun ido-yank ()
   "Select a kill to yank with `ido-completing-read'."
   (interactive)
   (insert-for-yank (ido-completing-read "Select kill: " kill-ring)))

(provide 'adh_ido)

;;; adh_ido.el ends here
