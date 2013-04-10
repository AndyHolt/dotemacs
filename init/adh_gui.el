;; don'adh_t show spash screen at startup
(setq inhibit-startup-message t)

;; cleanup gui of tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; turn off menu bar in windows (doesn't use any space in ubuntu)
(if (eq system-type 'windows-nt)
    (menu-bar-mode -1))

;; scrollbars on left (good way to see position in buffer!)
(set-scroll-bar-mode 'left)

;; column number in modeline
(column-number-mode t)

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Color Theme Setup
(load-theme 'wombat t)

;; Font selection - platform dependant
;;   Updated to work with server mode operation.
(cond ((eq system-type 'gnu/linux)
       (setq default-frame-alist '((font . "Monaco-11"))))
      ((eq system-type 'windows-nt)
       (setq default-frame-alist '((font . "Inconsolata-12")))))

;; modeline coloring
;;    better clarity of active and inactive window
;;    highlight buffer name
(set-face-background 'modeline-inactive "grey20")
(set-face-foreground 'modeline-inactive "grey75")
(set-face-background 'modeline "grey10")
(set-face-foreground 'modeline "grey75")
(set-face-foreground 'modeline-buffer-id "orange")

;; frame title to show file/buffer name
;; don't quite like this - to be tweaked later
(setq frame-title-format
  '("" invocation-name ": "(:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; show unfinished keystrokes quickly in minibuffer
(setq echo-keystrokes 0.1)

;; setup keybindings for easier window navigation
;; s-<arrow> used by Windows wm
;; no more C-x o!
(cond ((eq system-type 'gnu/linux)
       (global-set-key (kbd "s-<left>") 'windmove-left)
       (global-set-key (kbd "s-<right>") 'windmove-right)
       (global-set-key (kbd "s-<up>") 'windmove-up)
       (global-set-key (kbd "s-<down>") 'windmove-down))
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
(defvar babcore-fullscreen-p t "Check if fullscreen is on or off")
(setq babcore-stored-frame-width nil)
(setq babcore-stored-frame-height nil)

(defun babcore-non-fullscreen ()
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
  (interactive)
  (setq babcore-stored-frame-width (frame-width))
  (setq babcore-stored-frame-height (frame-height))
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun toggle-fullscreen ()
  (interactive)
  (setq babcore-fullscreen-p (not babcore-fullscreen-p))
  (if babcore-fullscreen-p
      (babcore-non-fullscreen)
    (babcore-fullscreen)))

(global-set-key [f11] 'toggle-fullscreen)

(provide 'adh_gui)
