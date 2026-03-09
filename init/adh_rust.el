;;; adh_rust.el --- Set up for rust programming language -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Fri 31 Oct 2025 00:33
;; URL: https://www.github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Configuration for developing in rust

;;; Code:

(require 'rust-mode-autoloads)

(eval-after-load 'rust-mode
  '(setq rust-mode-treesitter-derive t))

(provide 'adh_rust)
;;; adh_rust.el ends here
