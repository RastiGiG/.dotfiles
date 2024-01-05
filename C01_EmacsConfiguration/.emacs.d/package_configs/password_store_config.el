;; Add wrapper for the command line tool 'pass'
(use-package password-store
      :config
      ;; If you want to adjust the default password length
      ;; (setq password-store-password-length 12)

      ;; Use Password Store as Source for Auth-Sources
      (setq auth-sources '(password-store))

      ;; Add Functions to Leader Keys
      (pet/leader-keys
  	"ap"  '(:ignore t :which-key "Password Store")
  	"app" 'password-store-copy
  	"api" 'password-store-insert
  	"apg" 'password-store-generate))
