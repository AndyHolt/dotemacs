;;; adh_external.el --- Config for interface to external applications -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 28 Mar 2020 19:32
;; URL: https://github.com/AndyHolt/dotemacs

;;; Commentary:
;;
;; Configure settings required for interfacing Emacs with other applications

;;; Code:

;; google search from region or mini-buffer
;; code from prelude emacs setup: https://github.com/bbatsov/prelude
(defun google-search ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat 
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
		 (buffer-substring (region-beginning) (region-end))
		 (read-string "Google: " ))))))


;; search duckduckgo from region or minibuffer
;; could extend this idea to seach any ddg bang site from emacs!
(defun ddg-search ()
  "Search duckduckgo for a query or region if any.
  concatenates web-address with searched term after replacing spaces
  with + as required by ddg syntax"
  (interactive)
  (browse-url
   (concat
    "http://www.duckduckgo.com/?q="
    (replace-regexp-in-string " " "+"
	     (if mark-active
		 (buffer-substring (region-beginning) (region-end))
	         (read-string "Search ddg: " ))))))

;; set default browser to Chromium on linux systems
(cond
 ((eq system-type 'gnu/linux)
    (setq browse-url-browser-function (quote browse-url-chromium))))

(provide 'adh_external)
;;; adh_external.el ends here
