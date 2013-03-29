;; use ido mode
;;   for easy file and buffer finding
(ido-mode t)

;; don't change working directory when creating new files
;(setq ido-auto-merge-work-directories-length -1)

;; some basic config stuff - taked from Thomas Kjeldahl Nilsson
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;; ignore some buffers
(setq ido-ignore-buffers '(".*Completions\*" 
			   "^\*trace" 
			   "^\*compilation"
			   "\*Minibuf-\*"
			   ".*Echo\*"
			   ".*code-conversion-work.*"))

;; use ido almost everywhere
;(require 'ido-ubiquitous-mode)
(ido-ubiquitous-mode 1)

;; jump to home directory when using ido
;;   from whattheemacsd.com
(add-hook 'ido-setup-hook
  (lambda ()
    ;; Go straight home
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
	(interactive)
	(if(looking-back "/")
	    (insert "~/")
	  (call-interactively 'self-insert-command))))))

(provide 'adh_ido)
