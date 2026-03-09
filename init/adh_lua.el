;;; adh_lua.el --- Set up for lua programming language -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Fri 30 May 2025 10:46
;; URL: https://www.github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Configuration for writing Lua

;;; Code:

(require 'lua-mode-autoloads)

(eval-after-load 'eglot
  '(add-to-list 'eglot-server-programs
                `(lua-mode . ("lua-language-server"))))

(provide 'adh_lua)
;;; adh_lua.el ends here
