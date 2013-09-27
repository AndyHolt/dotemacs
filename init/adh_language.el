;; setup language, typing and associated stuff

;; add aspell to exec-path
(add-to-list 'exec-path "C:/Users/AHolt2/Aspell/bin")

;; dictionary setup
(setq-default ispell-program-name "aspell")

;; personal dictionary in aspell folder
(setq ispell-personal-dictionary "C:/Users/AHolt2/Aspell/.ispell")

(require 'ispell)

(provide 'adh_language)
