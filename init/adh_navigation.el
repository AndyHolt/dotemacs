;;; adh_navigation.el --- In buffer navigation setup -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:00
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up more powerful ways of navigating the current buffer.
;; Includes:
;;   - Avy - a replacement for ace-jump-mode
;;   - Goto line exention to show line numbers
;;   - Expand region and change-inner
;;   - Annoying arrows mode
;;   - Anzu mode

;;; Code:

;; avy setup
(autoload 'avy-goto-word-1 "avy" "" t)
(autoload 'avy-goto-char-timer "avy" "" t)
(autoload 'avy-goto-line "avy" "" t)

(setq avy-style 'at-full)
(setq avy-all-windows t)

;; setup keyboard shortcuts for avy mode (replacement for ace-jump mode)
(global-set-key "\C-cj" 'avy-goto-word-1)
(global-set-key "\C-ck" 'avy-goto-char-timer)
(global-set-key "\C-cg" 'avy-goto-line)
; [todo] - consider changing to avy-goto-subword-1 or avy-goto-word-or-subword-1
; (global-set-key "\C-cw" 'avy-goto-word-1)

;; setup ace-link mode, for jumping to links in help/info windows
; [todo] - why is ace-link-setup not working? Do I need it?
;;(ace-link-setup-default)

;; setup ace-window mode for fast selection of windows
(autoload 'ace-window "ace-window" "" t)
(global-set-key (kbd "C-x p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; show line numbers only when using 'goto-line'
(autoload 'display-line-numbers-mode "display-line-numbers" "" t)
;;   from whattheemacsd - thanks Magnars!
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let
      ((displaying-line-numbers display-line-numbers-mode))
      (unwind-protect
          (progn
            (display-line-numbers-mode 1)
            (goto-line (read-number "Goto line: ")))
        (display-line-numbers-mode (if displaying-line-numbers 1 -1)))))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; expand region by semantic units
(autoload 'er/expand-region "expand-region" "" t)
(global-set-key "\C-@" 'er/expand-region)

;; delete within semantic units (akin to expand-region)
(autoload 'change-inner "change-inner" "" t)
(autoload 'change-outer "change-inner" "" t)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; annoying arrows mode - annoy me if I'm navigating badly
;; (require 'annoying-arrows-mode)
;; (global-annoying-arrows-mode)

;; show number of search matches in mode line
;; note: swiper doesn't need this, can remove if swiper completely replaces
;; isearch usage
; (require 'anzu)
; (global-anzu-mode t)

;; set up swiper, a more powerful search
(autoload 'swiper "swiper" "" t)
(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)
;; use character folding in searches by default
;; (makes a match à, á, ã, â etc, and (significantly), α will match ἀ, ἁ, ᾶ, ά
;; ᾳ, etc)
(setq search-default-mode #'char-fold-to-regexp)

;; counsel-yank-pop
(global-set-key (kbd "C-x y") 'counsel-yank-pop)

;; save clipboard entry before killing to ensure that clipboard content from
;; external character is not lost if something in emacs is killed before
;; pasting.
(setq save-interprogram-paste-before-kill t)

;; keybinding for comment-region
;[todo] - perhaps move to another file?
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)

;; Don't show bookmark icon in fringe
(setq bookmark-set-fringe-mark 'nil)

;; automatically save bookmarks upon editing. Do not wait until Emacs is killed
;; (because if it closes unexpectedly or abnormally, bookmark changes will be
;; lost)
(setq bookmark-save-flag 1)

(provide 'adh_navigation)
;;; adh_navigation.el ends here
