; use smartparens mode instead of auto-pair and show-paren-mode
(smartparens-global-mode t)
(require 'smartparens-config)
(show-smartparens-global-mode t)

;; (sp-with-modes '(tex-mode
;;                  plain-tex-mode
;;                  latex-mode)
;;   (sp-local-pair "\\$" nil :actions nil)
;;   (sp-local-pair "$" "$"))

(provide 'adh_smartparens)
