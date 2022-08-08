;;; early-init.el --- Emacs config to load before startup.el -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 06 Oct 2020 23:25
;; URL: URL

;;; Commentary:
;;
;; Early emacs configuration, to be called becore startup.el is run.
;;
;; This is to be used for package setup as from Emacs 27 onwards,
;;package-initialize is run before the main init.el file is called.

;;; Code:

;; increase garbage collection threshold during startup.
;; This speeds up startup by reducing the frequency of garbage collection.
(setq gc-cons-threshold (* 4000 1000 1000))

(defvar use-startup-timer nil "Non-nil displays init file load times.

If nil, load Emacs init file normally. If non-nil, display timing of loading
each part of the init file using the `with-timer' macro.")

(setq use-startup-timer nil)

(defmacro with-timer (name &rest body)
  "Evaluate BODY and display the time taken for evaluation.

This macro is useful for analysing Emacs start up time, provided each element of
  the init file is loaded using the macro.

If value of `use-startup-timer' is nil, just execute BODY without recording
  timing details or outputting via `message'. This makes loading cleaner when
  not investigating start up time."
  `(if use-startup-timer
       (let ((time (current-time)))
         ,@body
         (message "%s: %.06f" ,name (float-time (time-since time))))
     (progn ,@body)))

;; setup cask
;; (with-timer "Loading Cask"
;; (cond ((eq system-type 'gnu/linux)
;;        (require 'cask "~/.cask/cask.el"))
;;       ((eq system-type 'darwin)
;;        (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")))
;; (cask-initialize)
;; )

(with-timer "Loading package package"
(require 'package)
)

(with-timer "Setting up package archives"
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "https://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
)

(with-timer "Early init gui set up"
;; Settings here are applied before a frame is created, so turning off UI
;; elements saves them from being drawn at all.
;;
;; don't show spash screen at startup
(setq inhibit-startup-message t
      initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
)

(provide 'early-init)
;;; early-init.el ends here
