;; Use EBDB for contact management
(use-package ebdb
  :config
  ;; Set the source files for Contact DBs
  (setq ebdb-sources (list
  					(concat pet/home-dir "Contacts/default-contacts.db")
  					(concat pet/home-dir "Contacts/family.db")
  					(concat pet/home-dir "Contacts/work.db")
  					(concat pet/home-dir "Contacts/organizations.db")
  					(concat pet/home-dir "Contacts/mailing-lists.db")))

  ;; Access Menu through '.' in EBDB Buffer
  ;; (define-key ebdb-mode-map
  ;;		  "." 'hydra-ebdb-menu/body)

  ;; Specify the Display Format for Month and Day on Anniversaries
  ;; (setq ebdb-anniversary-md-format "%B %d")
  ;; Specify the Display Format for Year, Month and Day on Anniversaries
  ;; (setq ebdb-anniversary-ymd-format "%B %d, %Y")

  ;; Set Keybindings
  (pet/leader-keys
      "c"  '(:ignore t :which-key "Contacts")
      "co" '(ebdb-open :which-key "Open Contact Database")))
