;;; adh_gui.el --- Setup aesthetic elements -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:41
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up various elements of the visual appearance, such as colour themes, menu
;; bars, frame title etc.

;;; Code:

(with-timer "misc gui set up"
;; ;; don't show spash screen at startup
;; (setq inhibit-startup-message t)

;; ;; cleanup gui of tool bar
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))

;; ;; turn off menu bar in windows (doesn't use any space in ubuntu)
;; (if (eq system-type 'windows-nt)
;;     (menu-bar-mode -1))

;; ;; scrollbars on left (good way to see position in buffer!)
;; ;; [fix] - scroll bars still appear. Need to remove.
;; ;(set-scroll-bar-mode nil)
;; (scroll-bar-mode -1)

;; reduce fringe around each buffer
(set-fringe-mode 8)

;; column number in modeline
(column-number-mode t)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; turn the stupid bell off
(setq ring-bell-function 'ignore)

;; make sure cursor doesn't blink
(blink-cursor-mode 0)

;; show unfinished keystrokes quickly in minibuffer
(setq echo-keystrokes 0.1)

;; setup conservative scrolling (one line at a time)
(setq scroll-conservatively 10000
      scroll-step 1)

;; Remove 3D effect of modeline, keep it flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; don't use pop up dialog box gui
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; allow resizing of emacs window by pixel, rather than character size. This
;; allows it to be resized much more effectively by window managers.
(setq frame-resize-pixelwise t)

)

(with-timer "git-gutter-fringe"
;; setup git-gutter mode
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
)

(with-timer "fonts setup"
;; Font selection - platform dependant
;;   Updated to work with server mode operation.
;; (cond ((eq system-type 'gnu/linux)
;;        (setq default-frame-alist '((font . "Inconsolata-12")
;;                                    (background-mode . dark))))
;;       ((eq system-type 'windows-nt)
;;        (setq default-frame-alist '((font . "Inconsolata-12")
;;                                    (background-mode . dark)))))

;; (set-face-attribute 'default nil :font "Inconsolata"
;;                     :height 120)

;; (when (member "DejaVu Sans Mono" (font-family-list))
;;   (set-frame-font "Inconsolata-14" t t))

(when (member "Menlo" (font-family-list))
  (set-frame-font "Menlo" t t)
  (set-face-font 'fixed-pitch "Menlo"))

;; Hebrew font setup
(set-fontset-font "fontset-default" '(#x0590 . #x05FF)
                  (font-spec :name "Times New Roman" :size 20))
(set-fontset-font "fontset-default" '(#xFB1D . #xFB4F)
                  (font-spec :name "Times New Roman" :size 20))

;; set variable-pitch-mode font
(set-face-font 'variable-pitch "Iowan Old Style-13")

(defun adh-set-font-for-eww (&rest opt)
    "Set variable pitch font for use in eww mode.
Ignores OPT."
    (set-face-font 'variable-pitch "Iowan Old Style-14"))

(add-hook 'eww-mode-hook #'adh-set-font-for-eww)

; (set-face-font 'variable-pitch "Baskerville-14")
; (set-face-font 'variable-pitch "Garamond-14")
; (set-face-font 'variable-pitch "Times New Roman-14")

)

(with-timer "theme setup"
;; Color Theme Setup
;; Use theme changer to select theme based on time of day
(require 'theme-changer)
(setq calendar-location-name "Dundee, Scotland")
(setq calendar-latitude 56.5)
(setq calendar-longitude -3.0)
;; (setq calendar-location-name "Banchory, Scotland")
;; (setq calendar-latitude 57.05)
;; (setq calendar-longitude -2.49)
;; (setq calendar-location-name "Louisville, KY"
;;       calendar-latitude 38.25
;;       calendar-longitude -85.68)

; NB: These solarized theme settings must be applied *before* loading theme
(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-scale-outline-headlines nil)

;; Add theme directories to theme path for loading when required
(mapc (lambda (tpath) (add-to-list 'custom-theme-load-path tpath))
      (file-expand-wildcards "~/.emacs.d/.cask/*/elpa/*-theme-*"))

(autoload 'solarized-light "solarized-theme" "" t)
(autoload 'solarized-light "solarized-light-theme" "" t)
(autoload 'solarized-zenburn "solarized-zenburn-theme" "" t)

(change-theme 'solarized-light 'solarized-zenburn)
)

(with-timer "windmove and text-scale"
;; setup keybindings for easier window navigation
;; s-<arrow> used by Windows wm
;; no more C-x o!
(cond ((eq system-type 'gnu/linux)
       (global-set-key (kbd "M-<left>") #'windmove-left)
       (global-set-key (kbd "M-<right>") #'windmove-right)
       (global-set-key (kbd "M-<up>") #'windmove-up)
       (global-set-key (kbd "M-<down>") #'windmove-down))
      ((eq system-type 'windows-nt)
       (global-set-key (kbd "C-S-<left>") #'windmove-left)
       (global-set-key (kbd "C-S-<right>") #'windmove-right)
       (global-set-key (kbd "C-S-<up>") #'windmove-up)
       (global-set-key (kbd "C-S-<down>") #'windmove-down)))

;; font size changing keybindings
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
)

(with-timer "writeroom and full screen"
;; setup writeroom mode and focus mode for 'distraction free' writing
(autoload 'writeroom-mode "writeroom-mode" "" t)
(setq writeroom-width 90)
;; set writeroom inter-line spacing
(setq writeroom-extra-line-spacing 0)

;[todo] - make writeroom mode auto-load focus-mode properly...
(add-hook 'writeroom-mode-hook
          (lambda ()
            (if focus-mode
                (focus-mode 1)
                (focus-mode 0))))

;; fullscreen stuff - taken from babcore emacs setup
; fullscreen, taken from http://www.emacswiki.org/emacs/FullScreen#toc26
; should work for X und OSX with emacs 23.x (TODO find minimum version).
; for windows it uses (w32-send-sys-command #xf030) (#xf030 == 61488)
;; (defvar babcore-fullscreen-p t "Check if fullscreen is on or off.")
;; (setq babcore-stored-frame-width nil)
;; (setq babcore-stored-frame-height nil)

;; (defun babcore-non-fullscreen ()
;;   "Restore frame to non-fullscreen."
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;;       ;; WM_SYSCOMMAND restore #xf120
;;       (w32-send-sys-command 61728)
;;     (progn (set-frame-parameter nil 'width
;;                                 (if babcore-stored-frame-width
;;                                     babcore-stored-frame-width 82))
;;            (set-frame-parameter nil 'height
;;                                 (if babcore-stored-frame-height
;;                                     babcore-stored-frame-height 42))
;;            (set-frame-parameter nil 'fullscreen nil))))

;; (defun babcore-fullscreen ()
;;   "Go fullscreen."
;;   (interactive)
;;   (setq babcore-stored-frame-width (frame-width))
;;   (setq babcore-stored-frame-height (frame-height))
;;   (if (fboundp 'w32-send-sys-command)
;;       ;; WM_SYSCOMMAND maximaze #xf030
;;       (w32-send-sys-command 61488)
;;     (set-frame-parameter nil 'fullscreen 'fullboth)))

;; (defun toggle-fullscreen ()
;;   "Switch between fullscreen and non-fullscreen modes."
;;   (interactive)
;;   (setq babcore-fullscreen-p (not babcore-fullscreen-p))
;;   (if babcore-fullscreen-p
;;       (babcore-non-fullscreen)
;;     (babcore-fullscreen)))

;; (global-set-key [f11] 'toggle-fullscreen)

;; kill ring stuff. Placed here until it outgrows this file.
;; (when (require 'browse-kill-ring nil 'noerror)
;;   (browse-kill-ring-default-keybindings)

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
)

(with-timer "which-key"
; [review] - farm out to own file if keeping
(require 'which-key)
(which-key-mode)
)

(with-timer "unsetting keys"
;; disable some keybindings that I don't want to use and are annoying.
(if (eq system-type 'darwin)
    (progn
      ; I have never once needed the system font panel in Emacs
      (global-unset-key (kbd "s-t"))
      ; I'll use the window manager to manage windows, and this keybinding is
      ; far to easy to accidentally hit and have the frame disappear
      (global-unset-key (kbd "C-z"))
      (global-unset-key (kbd "C-x C-z"))
      ; I'll use Emacs's own methods to quit, so don't enable macOS shortcuts
      ; for that
      (global-unset-key (kbd "s-w"))
      (global-unset-key (kbd "s-q"))))
)

(with-timer "highlight-indent mode"
;; highlight indent guides mode
(autoload 'highlight-indent-guides-mode "highlight-indent-guides" "" t)
(setq highlight-indent-guides-method 'bitmap)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

;; loading of visual-fill-column
(autoload 'visual-fill-column-mode "visual-fill-column")

(provide 'adh_gui)
;;; adh_gui.el ends here
