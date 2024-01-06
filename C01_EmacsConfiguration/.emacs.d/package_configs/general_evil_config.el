;; Setup general for easier key config
(use-package general
  :config
  ;; Enable evil mode integration
  (general-evil-setup)

  (general-create-definer pet/leader-keys
    :states '(normal insert visual emacs) 
    :prefix "SPC" ;; leader key
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; `kj` to get to normal mode
  (general-imap "k"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'evil-normal-state))
  ;; `jk` to get to normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  (pet/leader-keys
    ;; Toggles - Modes
    "tmvi"   '(evil-mode :which-key "Evil Mode")))
