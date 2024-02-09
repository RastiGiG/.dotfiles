;; Setup general for easier key config
(use-package general
  :config
  (general-create-definer pet/leader-keys
   :states '(normal insert visual emacs)
   :prefix "C-." ;; leader key
   :global-prefix "C-.") ;; access leader in insert mode
  )
