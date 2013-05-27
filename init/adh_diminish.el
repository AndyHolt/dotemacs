;; setup mode line to display less about minor modes
(require 'diminish)

(when (require 'diminish nil 'noerror)
  (eval-after-load "abbrev"
    (diminish 'abbrev-mode "Ab"))
  (eval-after-load "yasnippet"
    (diminish 'yas-minor-mode "ys"))
  (eval-after-load "flyspell"
    (diminish 'flyspell-mode "Fl")))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq mode-name "el")))

(provide 'adh_diminish)
