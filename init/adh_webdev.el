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
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))


;; be reasonable
(setq js-indent-level 2
      css-indent-offset 2)



(provide 'adh_webdev)
;;; adh_webdev.el ends here
