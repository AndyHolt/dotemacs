;;; adh_mode.el --- Set up modes

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Thu 17 Apr 2014 00:26
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Set up associations and settings for various major and minor modes.
;; In-mode settings are done in individual mode files, this file handles the
;; modes themselves.
;; Setup mode hooks and file-mode associations.

;;; Code:

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

;; use emacs lisp mode for Cask files
(setq auto-mode-alist
      (cons'("Cask" . emacs-lisp-mode) auto-mode-alist))

;; use conf-mode for watson config files
(setq auto-mode-alist
      (cons'(".watsonrc" . conf-mode) auto-mode-alist))

;; PDFs shouldn't be opened in Emacs. Load in hex mode as it's much faster.
;; [todo] - change to auto-open PDFs in standard program instead?
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . hexl-mode))

;; use markdown mode for .md files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; auctex mode when loading file with .tex extension.
;; [todo] - Need to setup auctex properly. Not in package repo just now?


(add-hook 'prog-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (show-paren-mode 1)
            (flyspell-prog-mode 1)))

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

;;; adh_mode.el ends here
