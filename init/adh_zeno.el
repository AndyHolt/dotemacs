;;; adh_zeno.el --- Online lookup helps -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Mon 08 Aug 2022 13:01
;; URL: 

;;; Commentary:
;;
;; Various functions which allow fast look up of various online references.
;; These are focused on biblical texts, mainly original languages. And currently
;; a lookup of LSJ dictionary through Logeion. In the future, other reference
;; sources may be added.

;; TODO:
;; - Perhaps, make a helm function that allows selection of chapters (save on
;;   typos when picking a chapter)
;; - Add a function to look up in LSJ online, including setting keymap for
;;   interactive input of chapter
;; - Add function for searching GNT/HB/LXX for terms via STEP

;;; Code:

;; On MacOS, use the open program to open urls in system default browser
(setq browse-url-generic-program "open")

;; Lookup functions
(defun browser-open-bible-text-na28 (chapter)
  "Open specified CHAPTER at academic-bible.com in NA28 text."
  (interactive "sChapter: ")
  (browse-url-generic (format "https://www.academic-bible.com/bible-text/%s/NA/"
                              (url-hexify-string chapter))))

(defun browser-open-bible-text-bhs (chapter)
  "Open specified CHAPTER at academic-bible.com in BHS text."
  (interactive "sChapter: ")
  (browse-url-generic (format "https://www.academic-bible.com/bible-text/%s/BHS/"
                              (url-hexify-string chapter))))

(defun browser-open-bible-text-lxx (chapter)
  "Open specified CHAPTER at academic-bible.com in LXX text."
  (interactive "sChapter: ")
  (browse-url-generic (format "https://www.academic-bible.com/bible-text/%s/LXX/"
                              (url-hexify-string chapter))))

(defun browser-open-step-gnt (chapter)
  "Open specified CHAPTER at stepbible.org in GNT."
  (interactive "sChapter: ")
  (browse-url-generic
   (format "https://www.stepbible.org/?q=reference=%s%%7Cversion=THGNT&options=VGNUP&skipwelcome"
           (url-hexify-string chapter))))

(defun browser-open-step-hb (chapter)
  "Open specified CHAPTER at stepbible.org in Hebrew Bible."
  (interactive "sChapter: ")
  (browse-url-generic
   (format "https://www.stepbible.org/?q=reference=%s%%7Cversion=THOT&options=VGNUP&skipwelcome"
           (url-hexify-string chapter))))

(defun browser-open-step-lxx (chapter)
  "Open specified CHAPTER at stepbible.org in LXX."
  (interactive "sChapter: ")
  (browse-url-generic
   (format "https://www.stepbible.org/?q=reference=%s%%7Cversion=LXX&options=VGNUP&skipwelcome"
           (url-hexify-string chapter))))

; todo - make interactive getting of word automatically set input method in
; minibuffer prompt. To do this, likely need to use a function for the
; interactive form, rather than simply a string input. There is also the "M"
; option which *does* inherit the current input method, but this would need to
; be set before calling the function. So perhaps a helper function could be
; added which sets the input method, then calls a version of
; browser-open-logeion-search which uses the "m" option for the interactive
; component.
(defun browser-open-logeion-search (word)
  "Open specified WORD at Logeion interface to Perseus."
  (interactive "sWord: ")
  (browse-url-generic
   (format "https://logeion.uchicago.edu/%s"
           word)))

;; define hydra for quick access to lookup functions
(require 'hydra)
(defhydra hydra-zeno-open (:color teal
                           :hint nil)
  "
^Original Texts^       ^STEP^       ^Langauge Tools^
^^^^^^^^--------------------------------------------
_n_: NA28              _g_: GNT     _w_: Logeion
_b_: BHS               _h_: HB
_l_: LXX               _o_: LXX
"
  ("n" browser-open-bible-text-na28)
  ("b" browser-open-bible-text-bhs)
  ("l" browser-open-bible-text-lxx)
  ("g" browser-open-step-gnt)
  ("h" browser-open-step-hb)
  ("o" browser-open-step-lxx)
  ("w" browser-open-logeion-search))

(define-key global-map (kbd "C-x /") 'hydra-zeno-open/body)


(provide 'adh_zeno)
;;; adh_zeno.el ends here
