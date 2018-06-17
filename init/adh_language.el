;;; adh_language.el --- Set up language, typing, dictionaries etc

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sun 23 Mar 2014 16:55
;; URL: https://github.com/AndyHolt/dotemacs/

;;; Commentary:
;;
;; Set up of features for language, spell check, typing and associated.

;;; Code:

;; dictionary setup
(setq-default ispell-program-name "aspell")

(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check CamelCase words"
  (interactive)
  (let (args)
    (cond
     ((string-match "aspell$" ispell-program-name)
      ;; force british english dictionary, support CamelCase spell checking
      (setq args (list "--sug-mode=ultra" "--lang=en_GB"))
      (if RUN-TOGETHER
          (setq args (append args '("--run-together" "--run-together-limit=5"
                                    "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

(setq ispell-extra-args (flyspell-detect-ispell-args t))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word
                                              activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(provide 'adh_language)

;;; adh_language.el ends here
