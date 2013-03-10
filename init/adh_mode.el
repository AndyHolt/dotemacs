(provide 'adh_mode)

;; octave major mode when loading file with .m extension
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons'("\\.m$" . octave-mode) auto-mode-alist))

;; auctex mode when loading file with .tex extension.


(add-hook 'octave-mode-hook
	  (lambda ()
	    ;(abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (reftex-mode 1)
	    (describe-function 'reftex-mode)
	    (auto-fill-mode 1)
	    (flyspell-mode 1)
	    (tex-pdf-mode 1)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (flyspell-mode 1)
	    (org-indent-mode 1)))
