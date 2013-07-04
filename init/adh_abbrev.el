; personal abbreviations
(define-abbrev-table 'global-abbrev-table '(

   ("LORD" "Lᴏʀᴅ")
   ("LORD's" "Lᴏʀᴅ's")

   ("8ra" "→")
   ("8la" "←")

   ("8w" "with")
   ("8bc" "because")
   ("8wo" "without")
   ("8tf" "∴")

   ("81\/2" "½")
   
   ;; Chinese
   ("8cn" "安迪")

   ;; email
   ("8hm" "andrew.holt@hotmail.co.uk")
   ("8gm" "andrew.holt635@gmail.com")
   ("8um" "ah635@cam.ac.uk")

   ("8contact" "Andrew D. Holt

(Home)                 (Term)
Whitlam Farmhouse      Emmanuel College
Newmachar              St. Andrew's Street
Aberdeen               Cambridge
AB21 0RS               CB2 3AP

Telephone Number: 07932 336416
Email: andrew.holt@hotmail.co.uk
Facebook: https://www.facebook.com/andydholt
Twitter: @theotherholt
Skype: andy.holt8
Github: https://github.com/adh635")

))

;; don't ask whether to save newly added abbrevs when quiting
(setq save-abbrevs nil)

;; turn abbrev mode on gloabally
(setq-default abbrev-mode t)

(provide 'adh_abbrev)
