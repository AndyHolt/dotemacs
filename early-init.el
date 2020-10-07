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
(setq gc-cons-threshold (* 100 1000 1000))

;; setup cask
(cond ((eq system-type 'gnu/linux)
       (require 'cask "~/.cask/cask.el"))
      ((eq system-type 'darwin)
       (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")))
(cask-initialize)

;; add marmalade repo to package.el list
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
            '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)


(provide 'early-init)
;;; early-init.el ends here
