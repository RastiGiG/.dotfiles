;; Setup general for easier key config
(use-package general
  :config
  ;; Enable evil mode integration
  (general-evil-setup)

  (general-create-definer pet/leader-keys
   :states '(normal insert visual emacs) 
   :prefix "SPC" ;; leader key
   :global-prefix "M-SPC") ;; access leader in insert mode

  (pet/leader-keys
    ;; Toggles - Modes
    "tmvi"   '(evil-mode :which-key "Evil Mode")))
