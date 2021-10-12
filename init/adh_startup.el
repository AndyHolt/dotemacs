;;; adh_startup.el --- Start up functions -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 17:23
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Things to do when starting Emacs.
;; Most of these should probably be moved into more specific files.

;;; Code:

(setq-default
  fill-column        80
  indent-tabs-mode   nil)

(global-font-lock-mode   t)

(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(auto-compression-mode   t)
(auto-image-file-mode    t)

(setq
 size-indication-mode  t
 require-final-newline t
 visible-cursor        nil)

(setq-default indicate-empty-lines t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; easily navigate camelCase etc words
(global-subword-mode 1)

;; (find-file (concat adh-dropbox-location "Org_files/todo.org"))

;; always prefer utf-8
(setq local-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; load auto-pair-mode globally.
;(require 'autopair)
;(autopair-global-mode)

;; moving to early-init
;; ;; don't show an initial startup message
;; (setq initial-scratch-message nil)

;; display startup time information
;; mainly for comparison of startup time with my config files compared with a
;; clean emacs instance.
;; See https://blog.d46.us/advanced-emacs-startup/
;; use a hook so that the message doesn't get clobbered by other messages
(defun adh-emacs-startup-message ()
  "Print a message about startup time and gc during Emacs startup."
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time (time-subtract after-init-time
                                              before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'adh-emacs-startup-message 90)

(provide 'adh_startup)
;;; adh_startup.el ends here
