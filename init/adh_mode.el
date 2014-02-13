(add-hook 'before-save-hook 'whitespace-cleanup)

;; matlab major mode when loading file with .m extension
(setq auto-mode-alist
      (cons'("\\.m$" . matlab-mode) auto-mode-alist))

;; set matlab indent level
(setq matlab-indent-level 2)
(setq matlab-cont-level 2)

;; use shell script mode for zshrc file
(setq auto-mode-alist
      (cons'("zshrc" . shell-script-mode) auto-mode-alist))

;; auctex mode when loading file with .tex extension.
(add-hook 'prog-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (show-paren-mode 1)))

(add-hook 'matlab-mode-hook
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
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (tex-pdf-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (org-indent-mode 1)))

(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (flyspell-mode 1)))

(add-to-list 'load-path
             "~/.emacs.d/plugins/")

(require 'tbemail)

(add-hook 'tbemail-mode
          (lambda ()
            (flyspell-mode 1)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)))

(provide 'adh_mode)
