;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:17

;;; Commentary:
;;
;; Contains my configuration for Emacs.
;; Works with Emacs 24 and later.
;;

;;; Code:

(with-timer "load-path config"
;; (add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/init/")
;; (add-to-list 'load-path "~/.emacs.d/.cask/")
(add-to-list 'load-path "~/Projects/Zenodotus/")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/.cask/"))
  (normal-top-level-add-subdirs-to-load-path))
)

;; disable handling file name of my start up files
;; This saves running a series of regexps against the file names to determine if
;; some kind of file handling is required. Takes a few miliseconds off init load
;; time, for no cost.
(let ((file-name-handler-alist nil))
;; load gcmh first as it should speed up load time
(with-timer "loading adh_gcmh" (require 'adh_gcmh))
(with-timer "loading adh_gui" (require 'adh_gui))
(with-timer "loading adh_org" (require 'adh_org))
;(require 'adh_ido)
(with-timer "loading adh_ivy" (require 'adh_ivy))
(with-timer "loading adh_yasnippet" (require 'adh_yasnippet))
(with-timer "loading adh_helm" (require 'adh_helm))
(with-timer "loading adh_mode" (require 'adh_mode))
(with-timer "loading adh_language" (require 'adh_language))
(with-timer "loading adh_buffersandfiles" (require 'adh_buffersandfiles))
(with-timer "loading adh_startup" (require 'adh_startup))
(with-timer "loading adh_external" (require 'adh_external))
(with-timer "loading adh_maths" (require 'adh_maths))
;; (require 'adh_smartmx)
(with-timer "loading adh_dired" (require 'adh_dired))
(with-timer "loading adh_abbrev" (require 'adh_abbrev))
(with-timer "loading adh_sessions" (require 'adh_sessions))
(with-timer "loading adh_navigation" (require 'adh_navigation))
;; (require 'adh_thunderlink)
(with-timer "loading adh_keyfreq" (require 'adh_keyfreq))
(with-timer "loading adh_hippie" (require 'adh_hippie))
;; (require 'adh_alias)
(with-timer "loading adh_flycheck" (require 'adh_flycheck))
;(require 'adh_sentence-highlight)
(with-timer "loading adh_projectile" (require 'adh_projectile))
;(require 'adh_ess)
(with-timer "loading adh_multiplecursors" (require 'adh_multiplecursors))
;(require 'adh_discover)
(with-timer "loading adh_smartparens" (require 'adh_smartparens))
(with-timer "loading adh_visualregexp" (require 'adh_visualregexp))
(with-timer "loading adh_latex" (require 'adh_latex))
(with-timer "loading adh_magit" (require 'adh_magit))
(with-timer "loading adh_fullframe" (require 'adh_fullframe))
(with-timer "loading adh_deft" (require 'adh_deft))
(with-timer "loading adh_hydra" (require 'adh_hydra))
(with-timer "loading adh_email" (require 'adh_email))
; (require 'adh_info)
; (require 'adh_orgcal)
; (require 'adh_pdf)

(with-timer "system specific config"
;; system specific configurations
(cond ((eq system-type 'gnu/linux)
       (require 'adh_linux))
      ((eq system-type 'darwin)
       (require 'adh_macos)))
)

(with-timer "load custom file"
;; don't let customize mess up my config files
(setq custom-file "~/.emacs.d/init/adh_custom.el")
(load custom-file 'noerror)
)

;; load diminish near the end to ensure everything is properly diminished
(with-timer "setting up diminish"
(require 'adh_diminish)
)


(with-timer "allow narrow to region"
(put 'narrow-to-region 'disabled nil)
)

(server-start)

;; after startup, decrease size of garbage collection threshold to make gc
;; pauses shorter. Add to emacs-startup-hook so that the threshold change comes
;; after handling commmand line options, notably --daemon
(defun adh-set-gc-threshold-for-normal-running ()
  "Set `gc-cons-threshold' for normal Emacs operation after startup.
A higher value is used during start up to speed up the startup. But use a lower
value when running Emacs to reduce the time taken to do a garbage collection.
This value likely will need to be tuned to reach the optimal value to balance
frequency of garbage collections and the time taken to do them."
  (with-timer "final thing: define gc-cons-threshold"
              (setq gc-cons-threshold (* 2000000))))
(add-hook 'emacs-startup-hook #'adh-set-gc-threshold-for-normal-running 80)

(message "End of init.el")
(provide 'init)

;;; init.el ends here
