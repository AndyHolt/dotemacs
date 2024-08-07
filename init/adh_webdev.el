;;; adh_webdev.el --- Settings for web dev -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Tue 15 Aug 2023 15:50
;; URL: https://github.com/AndyHolt/dotemacs/blob/main/init/adh_webdev.el

;;; Commentary:
;;
;; Settings for web development, including HTML, CSS and JavaScript, as well as
;; settings for other related environments (React)

;;; Code:

;; use treesitter mode for js files
(autoload 'js-ts-mode "js" "" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

;; set up web mode for .vue files
(autoload 'web-mode "web-mode" "" t)
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vls" "--stdio"))))

(setq js-indent-level 4
      css-indent-offset 2
      web-mode-script-padding 0
      web-mode-style-padding 0)


;; treesitter mode for typescript and tsx files
(autoload 'typescript-ts-mode "typescript-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(autoload 'tsx-ts-mode "typescript-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(provide 'adh_webdev)
;;; adh_webdev.el ends here
