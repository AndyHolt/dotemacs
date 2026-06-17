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
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

;; Set up vue-ts-mode for Vue files
(autoload 'vue-ts-mode "vue-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
(with-eval-after-load 'vue-ts-mode
  (setq vue-ts-mode-indent-offset 2))

(setq js-indent-level 2
      css-indent-offset 2
      web-mode-script-padding 0
      web-mode-style-padding 0)


;; treesitter mode for typescript and tsx files
(require 'typescript-mode-autoloads)
(autoload 'typescript-ts-mode "typescript-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(autoload 'tsx-ts-mode "typescript-ts-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(with-eval-after-load 'typescript-ts-mode
  (setq typescript-ts-mode-indent-offset 2))

(provide 'adh_webdev)
;;; adh_webdev.el ends here
