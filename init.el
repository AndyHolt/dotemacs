;;; init.el --- Emacs configuration.

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:17

;;; Commentary:
;;
;; Contains my configuration for Emacs.
;; Works with Emacs 24 and later.
;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/init")

;; setup cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; add marmalade repo to package.el list
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'adh_gui)
(require 'adh_org)
(require 'adh_ido)
(require 'adh_yasnippet)
(require 'adh_mode)
(require 'adh_language)
(require 'adh_buffersandfiles)
(require 'adh_startup)
(require 'adh_external)
(require 'adh_maths)
(require 'adh_smartmx)
(require 'adh_dired)
(require 'adh_abbrev)
(require 'adh_sessions)
(require 'adh_navigation)
(require 'adh_thunderlink)
(require 'adh_keyfreq)
(require 'adh_hippie)
(require 'adh_alias)
(require 'adh_flycheck)
(require 'adh_sentence-highlight)
(require 'adh_helm)
(require 'adh_projectile)
(require 'adh_diminish)
(require 'adh_ess)
(require 'adh_multiplecursors)
(require 'adh_discover)
(require 'adh_smartparens)
(require 'adh_visualregexp)

;; don't let customize mess up my config files
(setq custom-file "~/.emacs.d/init/adh_custom.el")
(load custom-file 'noerror)

(put 'narrow-to-region 'disabled nil)

(provide 'init)

;;; init.el ends here
