;;; adh_gptel.el --- Config for gptel LLM interface package -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Wed 05 Mar 2025 11:46
;; URL: https://github.com/AndyHolt/dotemacs/blob/main/init/adh_gptel.el

;;; Commentary:
;;
;; Configuration of the gptel package to make LLM APIs simple to use within Emacs.

;;; Code:

(require 'gptel)
;; (require 'gptel-curl)
(require 'gptel-anthropic)
(require 'transient)

(setopt gptel-default-mode 'org-mode
        gptel-org-branching-context t)

;; (setq gptel-model 'claude-sonnet-4-20250514)

(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

(let ((api-key (plist-get
                (car (auth-source-search :host "api.anthropic.com"
                                         :user "apikey"
                                         :require '(:user :secret)))
                :secret)))
  (setq gptel-model 'claude-sonnet-4-5-20250929
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (funcall api-key))))

(provide 'adh_gptel)
;;; adh_gptel.el ends here
