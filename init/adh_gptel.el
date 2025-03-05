;;; adh_gptel.el --- Config for gptel LLM interface package -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Wed 05 Mar 2025 11:46
;; URL: https://github.com/AndyHolt/dotemacs/blob/main/init/adh_gptel.el

;;; Commentary:
;;
;; Configuration of the gptel package to make LLM APIs simple to use within Emacs.

;;; Code:

(require 'gptel)
(require 'gptel-curl)
(require 'gptel-anthropic)

(setq gptel-default-mode 'org-mode)

(let ((api-key (plist-get
                (car (auth-source-search :host "api.anthropic.com"
                                         :user "apikey"
                                         :require '(:user :secret)))
                :secret)))
  (gptel-make-anthropic "Claude"
                        :stream t
                        :key (funcall api-key)))

(provide 'adh_gptel)
;;; adh_gptel.el ends here
