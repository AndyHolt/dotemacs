;;; adh_helm.el --- helm setup

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

(global-set-key (kbd "C-c h") 'helm-mini)

(provide 'adh_helm)

;;; adh_helm.el ends here
