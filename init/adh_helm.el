;;; adh_helm.el --- helm setup -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Fri 28 Mar 2014 14:50
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Helm is an incremental completion and selection narrowing framework for
;; Emacs.  (https://github.com/emacs-helm/helm)

;;; Code:

(require 'helm)
(require 'helm-config)

;; add extra sources to helm-mini
;; In addition to current buffers and recent files, other useful sources will
;; be:
;; - bookmarks
;; - Project buffers
;; - Projectile files
;; - Projectile projects
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-bookmarks
                                  helm-source-projectile-buffers-list
                                  helm-source-projectile-files-list
                                  helm-source-projectile-projects
                                  helm-source-buffer-not-found))

(global-set-key (kbd "C-c h") 'helm-mini)

;; load helm-bible
(add-to-list 'load-path "~/Projects/helm-bible/")
(require 'helm-bible)
(global-set-key (kbd "C-c m g") 'helm-bible)

;; load helm-browser-tabs
(add-to-list 'load-path "~/Projects/Zenodotus/")
(require 'helm-browser-tabs)
(global-set-key (kbd "C-c m t") 'helm-browser-tabs-with-presel)

;; Ensure that if lines are wrapped they wrap at spaces instead of in the middle
;; of words
(add-hook 'helm-major-mode-hook (lambda () (toggle-word-wrap t)
                                  (visual-fill-column-mode nil)
                                  (set-fill-column 1000)))

(provide 'adh_helm)
;;; adh_helm.el ends here
