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
; (require 'git-gutter-fringe+)
(autoload 'git-gutter+-mode "git-gutter-fringe+" "" t)
; (global-git-gutter+-mode t)
(defun enable-ggm-if-vc ()
  "Enable git-gutter+-mode if current buffer is in version control directory.

For use in mode hooks."
  (if (vc-root-dir)
      (git-gutter+-mode)
    nil))

(add-hook 'prog-mode-hook 'enable-ggm-if-vc)
(add-hook 'org-mode-hook 'enable-ggm-if-vc)
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
;;
;; N.B. use Times New Roman for Hebrew font because SBL BibLit and Ezra SIL do
;; not properly align pointing with consonants in display in Emacs (not sure
;; why, but no matter since Times New Roman works fine and is a decent Hebrew
;; font).
;; (set-fontset-font "fontset-default" '(#x0590 . #x05FF)
;;                   (font-spec :name "Times New Roman" :size 18 :inherit t))
;; (set-fontset-font "fontset-default" '(#xFB1D . #xFB4F)
;;                   (font-spec :name "Times New Roman" :size 18 :inherit t))
(set-fontset-font "fontset-default" 'hebrew (font-spec :family "Times New Roman"
                                                       :size 18))
 ;; (set-fontset-font "fontset-default" '(#x0590 . #x05FF)
 ;;                   (font-spec :name "SBL BibLit" :size 20))
 ;; (set-fontset-font "fontset-default" '(#xFB1D . #xFB4F)
 ;;                   (font-spec :name "SBL BibLit" :size 20))

(setq adh-hebrew-font-default-size 18)
(setq adh-hebrew-font-default-family "Times New Roman")

(defun adh-set-hebrew-font-size (size)
  "Set hebrew font to given size."
  (set-fontset-font "fontset-default" 'hebrew (font-spec
                                               :family adh-hebrew-font-default-family
                                               :size  size)))


(defun adh-set-hebrew-font-size-relative (&rest args)
  "Set hebrew font size relative to main text.

Utilising text-scale-mode settings, update Hebrew text size,
relative to main text."
  (interactive)
  (adh-set-hebrew-font-size
   (if (zerop text-scale-mode-amount)
       adh-hebrew-font-default-size
     (if (>= text-scale-mode-amount 0)
         (ceiling (* text-scale-mode-step
                     text-scale-mode-amount
                     adh-hebrew-font-default-size))
       (ceiling (/ adh-hebrew-font-default-size
                   (* -1 text-scale-mode-step text-scale-mode-amount)))))))

(advice-add 'text-scale-increase :after #'adh-set-hebrew-font-size-relative)

;; set fonts for apparatus symbols
(set-fontset-font "fontset-default" '(#x1D504 . #x1D59F)
                  (font-spec :name "Apparatus SIL"))
(set-fontset-font "fontset-default" '#x214F
                  (font-spec :name "Apparatus SIL"))
(set-fontset-font "fontset-default" '#x2135
                  (font-spec :name "Apparatus SIL"))
; TODO: work out what this character range should be!
;; (set-fontset-font "fontset-default" '(#x212D . )
;;                   (font-spec :name "Apparatus SIL"))


;; set variable-pitch-mode font
; (set-face-font 'variable-pitch "Iowan Old Style-13")
; (set-face-font 'variable-pitch "Gentium Plus-14")
(set-face-font 'variable-pitch (font-spec :name "Iowan Old Style" :size 13))


; N.B. Setting variable pitch font to Iowan Old Style is very nice for
; readability, but does not display italic fonts properly. This can be remedied
; by setting the italic font to another, similar-ish font that *does* display
; italics, such as Gentium Plus. But need to work out a way of doing this only
; when variable-pitch-mode is active.
; (set-face-font 'italic "Gentium Plus-14")

(defun adh-set-font-for-eww (&rest opt)
    "Set variable pitch font for use in eww mode.
Ignores OPT."
    (set-face-font 'variable-pitch (font-spec :name "Iowan Old Style"
                                              :size 13)))

(add-hook 'eww-mode-hook #'adh-set-font-for-eww)

; (set-face-font 'variable-pitch "Baskerville-14")
; (set-face-font 'variable-pitch "Garamond-14")
; (set-face-font 'variable-pitch "Times New Roman-14")

)

(with-timer "theme setup"
;; Color Theme Setup
;; Use theme changer to select theme based on time of day
;; (require 'theme-changer)
(setq calendar-location-name "Dundee, Scotland")
(setq calendar-latitude 56.5)
(setq calendar-longitude -3.1)
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

;; (change-theme 'solarized-light 'solarized-zenburn)

; Use macOS dark and light mode to set colour theme of emacs. This keeps it in
; sync with the rest of the system.
(defun adh/apply-appropriate-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration.

If current system appearance is light mode, set an appropriate
  theme for daytime. If current system appearance is dark mode,
  use a suitable dark theme."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'solarized-light t))
    ('dark (load-theme 'solarized-zenburn t))))

(add-hook 'ns-system-appearance-change-functions #'adh/apply-appropriate-theme)

;; minor mode for handling MacOS frame titlebar colour
(autoload 'ns-auto-titlebar-mode "ns-auto-titlebar" "" t)
(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

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

;; handle font-scaling for italic text. By default, the text-scale commands do
;; not affect italic text. This is a fix from
;; https://emacs.stackexchange.com/questions/29511/text-scale-for-all-faces-in-buffer
;; which solves that.
(defun adh-face-remap-add-relative-advice (face &rest specs)
  "Advice to add text scale remap for italic font"
  (if (eq face 'default)
      (setq my/italic-cookie (apply #'face-remap-add-relative 'italic specs))))

(defun adh-face-remap-remove-relative-advice (cookie)
  "Advice to remove text scale remap for italic font"
  (if (eq (car cookie) 'default)
      (face-remap-remove-relative my/italic-cookie)))

(advice-add #'face-remap-add-relative :after #'adh-face-remap-add-relative-advice)
(advice-add #'face-remap-remove-relative :after #'adh-face-remap-remove-relative-advice)

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
