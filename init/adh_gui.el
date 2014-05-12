;;; adh_gui.el --- Setup aesthetic elements

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:41
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up various elements of the visual appearance, such as colour themes, menu
;; bars, frame title etc.

;;; Code:

;; don't show spash screen at startup
(setq inhibit-startup-message t)

;; cleanup gui of tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; turn off menu bar in windows (doesn't use any space in ubuntu)
(if (eq system-type 'windows-nt)
    (menu-bar-mode -1))

;; scrollbars on left (good way to see position in buffer!)
;; [fix] - scroll bars still appear. Need to remove.
;(set-scroll-bar-mode nil)
(scroll-bar-mode -1)

;; reduce fringe around each buffer
(set-fringe-mode 8)

;; setup git-gutter mode
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; column number in modeline
(column-number-mode t)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Color Theme Setup
;; Use theme changer to select theme based on time of day
;(load-theme 'wombat t)
;(color-theme-solarized-dark)
(require 'theme-changer)
(setq calendar-location-name "Cambridge, UK")
(setq calendar-latitude 52.2)
(setq calendar-longitude 0.1)
(change-theme 'solarized-light 'solarized-dark)

;; make sure cursor doesn't blink
(blink-cursor-mode 0)

;; Font selection - platform dependant
;;   Updated to work with server mode operation.
(cond ((eq system-type 'gnu/linux)
       (setq default-frame-alist '((font . "Inconsolata-12")
                                   (background-mode . dark))))
      ((eq system-type 'windows-nt)
       (setq default-frame-alist '((font . "Inconsolata-12")
                                   (background-mode . dark)))))

;; modeline coloring
;;    better clarity of active and inactive window
;;    highlight buffer name
;; commented out while using solarized theme.
;(set-face-background 'mode-line-inactive "grey20")
;(set-face-foreground 'mode-line-inactive "grey75")
;(set-face-background 'mode-line "grey10")
;(set-face-foreground 'mode-line "grey75")
;(set-face-foreground 'mode-line-buffer-id "orange")

;; frame title to show file/buffer name
;; don't quite like this - to be tweaked later
;; (setq frame-title-format
;;   '("" invocation-name ": "(:eval (if (buffer-file-name)
;;                 (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))
(setq frame-title-format
  '("" invocation-name))


;; show unfinished keystrokes quickly in minibuffer
(setq echo-keystrokes 0.1)

;; setup keybindings for easier window navigation
;; s-<arrow> used by Windows wm
;; no more C-x o!
(cond ((eq system-type 'gnu/linux)
       (global-set-key (kbd "M-<left>") 'windmove-left)
       (global-set-key (kbd "M-<right>") 'windmove-right)
       (global-set-key (kbd "M-<up>") 'windmove-up)
       (global-set-key (kbd "M-<down>") 'windmove-down))
      ((eq system-type 'windows-nt)
       (global-set-key (kbd "C-S-<left>") 'windmove-left)
       (global-set-key (kbd "C-S-<right>") 'windmove-right)
       (global-set-key (kbd "C-S-<up>") 'windmove-up)
       (global-set-key (kbd "C-S-<down>") 'windmove-down)))

;; font size changing keybindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; setup conservative scrolling (one line at a time)
(setq scroll-conservatively 10000
      scroll-step 1)


;; fullscreen stuff - taken from babcore emacs setup
; fullscreen, taken from http://www.emacswiki.org/emacs/FullScreen#toc26
; should work for X und OSX with emacs 23.x (TODO find minimum version).
; for windows it uses (w32-send-sys-command #xf030) (#xf030 == 61488)
(defvar babcore-fullscreen-p t "Check if fullscreen is on or off.")
(setq babcore-stored-frame-width nil)
(setq babcore-stored-frame-height nil)

(defun babcore-non-fullscreen ()
  "Restore frame to non-fullscreen."
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width
                                (if babcore-stored-frame-width
                                    babcore-stored-frame-width 82))
           (set-frame-parameter nil 'height
                                (if babcore-stored-frame-height
                                    babcore-stored-frame-height 42))
           (set-frame-parameter nil 'fullscreen nil))))

(defun babcore-fullscreen ()
  "Go fullscreen."
  (interactive)
  (setq babcore-stored-frame-width (frame-width))
  (setq babcore-stored-frame-height (frame-height))
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun toggle-fullscreen ()
  "Switch between fullscreen and non-fullscreen modes."
  (interactive)
  (setq babcore-fullscreen-p (not babcore-fullscreen-p))
  (if babcore-fullscreen-p
      (babcore-non-fullscreen)
    (babcore-fullscreen)))

(global-set-key [f11] 'toggle-fullscreen)

;; kill ring stuff. Placed here until it outgrows this file.
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; toggle between horizontal and vertical window split.
;; from whattheemacsd.com
(defun toggle-window-split ()
  "Toggle between horizontal and vertical window splits."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'adh_gui)

;;; adh_gui.el ends here
