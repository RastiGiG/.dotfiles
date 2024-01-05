;; Add Ledger Mode from Melpa
;; (Alternatively include the installation path of ledger to load-path)
(use-package ledger-mode
  :config
  ;; Add mode Toggle to Keyspace
  (pet/leader-keys
    "tml"   '(ledger-mode :which-key "Ledger Mode"))

  ;; Load mode on .dat files
  :mode "\\.dat\\'")
