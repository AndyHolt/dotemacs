;;; adh_language.el --- Set up language, typing, dictionaries etc -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:55
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up of features for language, spell check, typing and associated.

;;; Code:

;; dictionary setup
(setq-default ispell-program-name "aspell")

;; (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
;;   "if RUN-TOGETHER is true, spell check CamelCase words"
;;   (interactive)
;;   (let (args)
;;     (cond
;;      ((string-match "aspell$" ispell-program-name)
;;       ;; force british english dictionary, support CamelCase spell checking
;;       (setq args (list "--sug-mode=ultra" "--lang=en_GB"))
;;       (if RUN-TOGETHER
;;           (setq args (append args '("--run-together" "--run-together-limit=5"
;;                                     "--run-together-min=2")))))
;;      ((string-match "hunspell$" ispell-program-name)
;;       (setq args nil)))
;;     args
;;     ))

;; (cond
;;  ((executable-find "aspell")
;;   (setq ispell-program-name "aspell"))
;;  ((executable-find "hunspell")
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-local-dictionary "en_GB")
;;   (setq ispell-local-dictionary-alist
;;         '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
;;  (t (setq ispell-program-name nil)))

;; (setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (defadvice ispell-word (around my-ispell-word activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)
;;     ))

;; (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word
;;                                               activate)
;;   (let ((old-ispell-extra-args ispell-extra-args))
;;     (ispell-kill-ispell t)
;;     (setq ispell-extra-args (flyspell-detect-ispell-args))
;;     ad-do-it
;;     (setq ispell-extra-args old-ispell-extra-args)
;;     (ispell-kill-ispell t)
;;     ))

(autoload 'define-word "define-word" "" t)
(autoload 'define-word-at-point "define-word" "" t)
(global-set-key (kbd "C-c d") 'define-word)
(global-set-key (kbd "C-c D") 'define-word-at-point)

;; Bugfix for define-word
;; See Helinwang's suggested bugfix at:
;; https://github.com/abo-abo/define-word/issues/31
(defun url-http-user-agent-string ()
  "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.80 Safari/537.36
")

;; setup typo minor mode
;; Use typographical unicode more easily, e.g. en-rules
(autoload 'typo-mode "typo" "" t)

(add-hook 'mu4e-compose-mode-hook 'typo-mode)

;; by default, emacs considers a full stop with a single space to be an
;; abbreviation, while a full stop with a double space is a new sentence.
;; This behaviour doesn't suit anything I write! So change this to nil to make a
;; full stop with a single space counted as the end of a sentence. If
;; abbreviations ruin this, set to true again.
(setq sentence-end-double-space nil)

;; set standard alternative input method to Greek
(setq default-input-method "greek-babel")

;; Give easy access to both Greek and Hebrew typing
(defun adh-set-other-input-method ()
    "Set the input method to whichever of greek-babel and hebrew-biblical-sil is
    not currently the default input method. This allows quick switching between two
    regularly used input-methods.

    When there is currently no input method set (standard input), calling this
    function will set the input method to the other method than would be set by
    `toggle-input-method'. If an input method is set, this will switch to the
    other one."
  (interactive)
  (set-input-method (if (equal default-input-method "greek-babel")
                      "hebrew-biblical-sil"
                    "greek-babel")))

(global-set-key (kbd "C-|") #'adh-set-other-input-method)

(provide 'adh_language)
;;; adh_language.el ends here
