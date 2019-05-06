;;; init.el --- Emacs configuration.

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:17

;;; Commentary:
;;
;; Contains my configuration for Emacs.
;; Works with Emacs 24 and later.
;;

;;; Code:

;; (add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/init/")
;; (add-to-list 'load-path "~/.emacs.d/.cask/")
(let ((default-directory  "~/.emacs.d/.cask/"))
  (normal-top-level-add-subdirs-to-load-path))

;; setup cask
(cond ((eq system-type 'gnu/linux)
       (require 'cask "~/.cask/cask.el"))
      ((eq system-type 'darwin)
       (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")))
(cask-initialize)

;; add marmalade repo to package.el list
(require 'package)
(add-to-list 'package-archives
             '("gnu". "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
            '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; load gcmh first as it should speed up load time
(require 'adh_gcmh)
(require 'adh_gui)
(require 'adh_org)
;(require 'adh_ido)
(require 'adh_ivy)
(require 'adh_yasnippet)
(require 'adh_helm)
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
;(require 'adh_sentence-highlight)
(require 'adh_projectile)
(require 'adh_diminish)
;(require 'adh_ess)
(require 'adh_multiplecursors)
;(require 'adh_discover)
(require 'adh_smartparens)
(require 'adh_visualregexp)
(require 'adh_latex)
(require 'adh_magit)
(require 'adh_fullframe)
(require 'adh_deft)
(require 'adh_hydra)
(require 'adh_email)
(require 'adh_orgcal)
(require 'adh_pdf)

;; system specific configurations
(cond ((eq system-type 'gnu/linux)
       (require 'adh_linux))
      ((eq system-type 'darwin)
       (require 'adh_osx)))

;; don't let customize mess up my config files
(setq custom-file "~/.emacs.d/init/adh_custom.el")
(load custom-file 'noerror)

(put 'narrow-to-region 'disabled nil)

(server-start)

(message "End of init.el")
(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (package-build shut-up epl git commander f dash s))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
