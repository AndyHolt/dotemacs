;;; adh_tree-sitter.el --- tree-sitter config -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Fri 22 Sep 2023 23:20
;; URL: https://github.com/AndyHolt/dotemacs/blob/main/init/adh_tree-sitter.el

;;; Commentary:
;;
;; Configure tree-sitter for various languages.
;;
;; Install configured language grammars with `M-x
;; treesit-install-langauge-grammar'

;;; Code:

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (markdown "https://github.com/MDeiml/tree-sitter-markdown"
                  "split_parser" "tree-sitter-markdown/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/ikatyang/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (go-mod-mode . go-mod-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(customize-set-variable 'treesit-font-lock-level 3)

;; config for go-ts-mode
(defun adh/go-ts-activate-extra-yas-modes ()
  "Helper function for hook to enable go-mode snippets in `go-ts-mode'."
  (yas-activate-extra-mode 'go-mode))
(add-hook 'go-ts-mode-hook #'adh/go-ts-activate-extra-yas-modes)

(defun adh/go-ts-set-tab-width ()
  "Helper function for hook to set tab size to 4 in `go-ts-mode'."
  (setq tab-width 4))
(add-hook 'go-ts-mode-hook #'adh/go-ts-set-tab-width)

(setq go-ts-mode-indent-offset 4)

;; config for python-ts-mode
(defun adh/python-ts-activate-extra-yas-modes ()
  "Helper function for hook to enable `python-mode' snippets in `go-ts-mode'."
  (yas-activate-extra-mode 'python-mode))
(add-hook 'python-ts-mode-hook #'adh/python-ts-activate-extra-yas-modes)

;; config for js-ts-mode (Javascript)
(autoload 'js-ts-mode "js" "" t)



(provide 'adh_tree-sitter)
;;; adh_tree-sitter.el ends here
