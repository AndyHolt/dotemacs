;;; adh_email.el --- setup email from Emacs -*- lexical-binding: t -*-

;; Author: Andy Holt (andrew.holt@hotmail.co.uk)
;; Date: Sat 12 Mar 2016 17:11

;;; Commentary:
;;
;; Configuration for mu4e: emails in Emacs!

;;; Code:

;; add mu4e to load-path, installed by homebrew on mac so not in usual location
(add-to-list 'load-path
             "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e")
(autoload 'mu4e "mu4e")

;; set up my addresses
;; (setq mu4e-user-mail-address-list
;;       '("andrew.holt@hotmail.co.uk"
;;         "andrew.holt635@gmail.com"
;;         "andy.holt@cantab.net"
;;         "aholt379@students.sbts.edu"
;;         ))

;; Set up folders
(setq 
  mu4e-root-maildir  "~/.mail"                  ;; top level mail folder
  mu4e-sent-folder   "/ah635-gmail.com/sent"    ;; sent mail folder
  mu4e-drafts-folder "/ah635-gmail.com/drafts"  ;; drafts folder
  mu4e-trash-folder  "/ah635-gmail.com/Trash"   ;; trashed messages
  mu4e-refile-folder "/ah635-gmail.com/archive" ;; saved messages
)

;; getting mail
(setq 
  mu4e-get-mail-command "/Users/adh/Projects/mailsync/mailsync"
  mu4e-update-interval nil ;; don't update, this is handled by launchctl
)

;; sending mail
(setq
  user-mail-address "andrew.holt@hotmail.co.uk"
  user-full-name "Andy Holt")

;; sending mail via smstp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp" )
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

;; use w3m to display html content
(setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")

;; change formatting of reply message line:
(setq message-citation-line-format "On %d %b %Y at %H:%M, %f wrote:"
      message-citation-line-function 'message-insert-formatted-citation-line )

;; signatures
(setq mu4e-compose-signature-auto-include nil )
(setq mu4e-compose-signature "Andy Holt" )

;; setup contexts for different accounts
(eval-after-load "mu4e"
'(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "hotmail"
           :enter-func (lambda () (mu4e-message "Switch to hotmail context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                             :to "andrew.holt@hotmail.co.uk")))
           :vars '(  ( user-mail-address . "andrew.holt@hotmail.co.uk"  )
                     ( user-full-name . "Andy Holt" )
                     ;; ( mu4e-compose-signature-auto-include nil )
                     ( mu4e-compose-signature-auto-include . nil)
                     ( mu4e-compose-signature . "Andy Holt" )
                     ( smtpmail-stream-type . "ssl" )
                     ( smtpmail-default-smtp-server . "smtp-mail.outlook.com" )
                     ( smtpmail-smtp-server . "smtp-mail.outlook.com" )
                     ( smtpmail-smtp-service . 587 )
                     ( smtp-mail-smtp-user . "andrew.holt@hotmail.co.uk" )
                     ( mu4e-compose-reply-to-address . nil)
                     ( mu4e-sent-folder . "/hotmail/Sent" )
                     ( mu4e-drafts-folder . "/hotmail/Drafts" )
                     ( mu4e-trash-folder . "/hotmail/Deleted" )
                     ( mu4e-refile-folder . "/hotmail/Archive" )
                     ( mu4e-sent-messages-behavior . delete)))
         ,(make-mu4e-context
           :name "gmail"
           :enter-func (lambda () (mu4e-message "Switch to gmail context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                             :to "andrew.holt635@gmail.com")))
           :vars '(  ( user-mail-address . "andrew.holt635@gmail.com" )
                     ( user-full-name	    . "Andy Holt" )
                     ( mu4e-compose-signature-auto-include . nil)
                     ( mu4e-compose-signature . "Andy Holt" )
                     ( smtpmail-stream-type . 'starttls )
                     ( smtpmail-default-smtp-server . "smtp.gmail.com" )
                     ( smtpmail-smtp-server . "smtp.gmail.com")
                     ( smtpmail-smtp-service . 587 )
                     ( smtpmail-smtp-user . "andrew.holt635")
                     ( mu4e-compose-reply-to-address . nil)
                     ( mu4e-sent-folder   . "/ah635-gmail.com/sent"    )
                     ( mu4e-drafts-folder . "/ah635-gmail.com/drafts"  )
                     ( mu4e-trash-folder  . "/ah635-gmail.com/Trash"   )
                     ( mu4e-refile-folder . "/ah635-gmail.com/archive" )
                     ( mu4e-sent-messages-behavior . delete)))
         ,(make-mu4e-context
           :name "cantab"
           :enter-func (lambda () (mu4e-message "Switch to cantab context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                             :to "andy.holt@cantab.net")))
           :vars '(  ( user-mail-address . "andy.holt@cantab.net" )
                     ( user-full-name	    . "Andy Holt" )
                     ( mu4e-compose-signature-auto-include . nil)
                     ( mu4e-compose-signature . "Andy Holt" )
                     ( smtpmail-stream-type . "ssl" )
                     ( smtpmail-default-smtp-server . "mail.secure.aluminati.net" )
                     ( smtpmail-smtp-server . "mail.secure.aluminati.net")
                     ( smtpmail-smtp-service . 587 )
                     ( smtpmail-smtp-user . "andy.holt@cantab.net")
                     ( mu4e-compose-reply-to-address . nil)
                     ( mu4e-sent-folder   . "/cantab/Sent"    )
                     ( mu4e-drafts-folder . "/cantab/Drafts"  )
                     ( mu4e-trash-folder  . "/cantab/Trash"   )
                     ( mu4e-refile-folder . "/cantab/Archive" )
                     ( mu4e-sent-messages-behavior . sent)))
         ,(make-mu4e-context
           :name "sbts"
           :enter-func (lambda () (mu4e-message "Switch to SBTS context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                             :to "aholt379@students.sbts.edu")))
           :vars '(  ( user-mail-address . "aholt379@students.sbts.edu" )
                     ( user-full-name	    . "Andy Holt" )
                     ( mu4e-compose-signature-auto-include . nil)
                     ( mu4e-compose-signature . "Andy Holt" )
                     ( smtpmail-stream-type . 'starttls )
                     ( smtpmail-default-smtp-server . "smtp.gmail.com" )
                     ( smtpmail-smtp-server . "smtp.gmail.com")
                     ( smtpmail-smtp-service . 587 )
                     ( smtpmail-smtp-user . "aholt379@students.sbts.edu")
                     ( mu4e-compose-reply-to-address . nil)
                     ( mu4e-sent-folder   . "/sbts/sent"    )
                     ( mu4e-drafts-folder . "/sbts/drafts"  )
                     ( mu4e-trash-folder  . "/sbts/trash"   )
                     ( mu4e-refile-folder . "/sbts/archive" )
                     ( mu4e-sent-messages-behavior . delete)))
         )))

;; if not otherwise specified, use hotmail context as default
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
(setq mu4e-compose-context-policy 'ask)

;; headers view: configure headers
(setq mu4e-headers-date-format "%a %x" )
(setq mu4e-headers-fields '( (:human-date . 16)
                             (:flags . 6)
                             (:mailing-list . 10)
                             (:from-or-to . 22)
                             (:subject)) )


;; try using fancy characters for marks and threads...
;; don't.
(setq mu4e-use-fancy-chars nil)

;; show email addresses in From:, not just names
(setq mu4e-view-show-addresses t)

;; add shortcuts to inboxes
(setq mu4e-maildir-shortcuts
      '( (:maildir "/hotmail/Inbox" :key ?h)
         (:maildir "/ah635-gmail.com/INBOX" :key ?g)
         (:maildir "/cantab/INBOX" :key ?c)
         (:maildir "/sbts/inbox" :key ?s)
         ))

;; set up actions
;; view html mail in web browser
(eval-after-load "mu4e"
  '(add-to-list 'mu4e-view-actions
                '("bview in browser" . mu4e-action-view-in-browser) t))

;; after sending, kill buffer
;; (avoid getting a whole load of open buffers which I don't want/need)
(setq message-kill-buffer-on-exit 't)

;; ;; line wrapping -- soft line breaks when reading mail
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; (setq whitespace-display-mappings
;;       '((newline-mark 10 [?↷ 10])))

;; (eval-after-load 'whitespace
;;   (lambda ()
;;     (set-face-attribute 'whitespace-newline nil :foreground "#d3d7cf")))

;; (defvar my-visual-line-state nil)

;; (defun my-visual-line-mode-hook ()
;;   (when visual-line-mode
;;     (setq my-visual-line-state
;;           `(whitespace-style ,whitespace-style
;;             whitespace-mode ,whitespace-mode
;;             auto-fill-mode ,auto-fill-function))
;;     (when whitespace-mode
;;       whitespace-mode -1)
;;     (make-local-variable 'whitespace-style)
;;     (setq whitespace-style '(newline newline-mark))
;;     (whitespace-mode)
;;     (when auto-fill-function
;;       (auto-fill-mode -1))
;;     (visual-fill-column-mode)))

;; (add-hook 'visual-line-mode-hook 'my-visual-line-mode-hook)

;; (defun my-visual-line-mode-off ()
;;   (interactive)
;;   (visual-fill-column-mode--disable)
;;   (visual-line-mode -1)
;;   ;; revert the state before activating visual-line-mode
;;   (when my-visual-line-state
;;     (let ((ws-style (plist-get my-visual-line-state 'whitespace-style))
;;           (ws-mode (plist-get my-visual-line-state 'whitespace-mode))
;;           (af-mode (plist-get my-visual-line-state 'auto-fill-mode)))
      
;;       (when whitespace-mode
;;         (whitespace-mode -1))
;;       (when ws-style (setq whitespace-style ws-style))
;;       (when ws-mode (whitespace-mode 1))
;;       (when af-mode (auto-fill-mode 1)))))

;; (defun my-visual-line-mode-toggle ()
;;   (interactive)
;;   (if visual-line-mode
;;       (my-visual-line-mode-off)
;;     (visual-line-mode 1)))

;; (add-hook 'mu4e-view-mode-hook 'whitespace-mode)
;; (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

;; use format-flowed and soft line breaks when composing emails
(setq mu4e-compose-format-flowed t)

(defun adh-mu4e-compose-line-breaking ()
    "Set up preferred line breaking settings for composing emails
in mu4e. Use soft line breaks and ensure format-flowed is used.
This will make emails much nicer to read in external mail programmes."
    (auto-fill-mode 0)
    (visual-line-mode t)
    (visual-fill-column-mode t)
    (toggle-word-wrap t))

(add-hook 'mu4e-compose-mode-hook #'adh-mu4e-compose-line-breaking)

;; auto-fill-mode
;; visual-line-mode
;; visual-fill-column-mode
;; toggle-word-wrap

;; global keybinding to enter email
(global-set-key (kbd "C-c m m") 'mu4e)

;; change global keybinding C-x m to use mu4e for composition
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

;; turn on gnus-dired-attach to attach to emails from dired
(turn-on-gnus-dired-mode)

;; use org-mu4e for email links from org files
(eval-after-load "mu4e"
  '(require 'org-mu4e))

;; Don't include related emails in headers view
;; if non-nil (as set to default in mu 1.0), replies show in headers view of
;; inbox, etc
;; Can change value by pressing W in headers view
(setq mu4e-headers-include-related nil)

(eval-after-load "mu4e"
  '(progn
     ;; Add bookmark for searching for a particular date, date selected using
     ;; org-read-date
     (add-to-list 'mu4e-bookmarks
                  '(:name "Select date"
                          :query (lambda ()
                                   (let* ((org-read-date-prefer-future nil)
                                          (date (format-time-string "%Y%m%d"
                                                                    (org-read-date nil
                                                                                   t))))
                                     (concat "date:" date)))
                          :key ?d))

     ;; Add bookmark for today's emails in inboxes.
     ;; Want to see emails for today across all inboxes, like "Today's messages"
     ;; bookmark, but not ones which have been deleted or refiled already.
     (add-to-list 'mu4e-bookmarks
                  '(:name "Today's inbox"
                          :query (concat "(maildir:/hotmail/Inbox OR "
                                         "maildir:/cantab/INBOX OR "
                                         "maildir:/ah635-gmail.com/INBOX OR "
                                         "maildir:/sbts/inbox) "
                                         "AND date:today..")
                          :key ?i))

     (add-to-list 'mu4e-bookmarks
                  '(:name "Combined inbox"
                          :query (concat "maildir:/hotmail/Inbox OR "
                                         "maildir:/cantab/INBOX OR "
                                         "maildir:/ah635-gmail.com/INBOX OR "
                                         "maildir:/sbts/inbox")
                          :key ?c))))

;; Don't need to confirm quitting mu4e. Don't ask, just do it.
(setq mu4e-confirm-quit nil)

;; load file with personal mail list addresses.
(eval-after-load "mu4e"
  '(require 'adh_personal_email_lists))

(provide 'adh_email)
;;; adh_email.el ends here
