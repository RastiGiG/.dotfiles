;; adds git related prompt elements to eshell
(use-package eshell-git-prompt)

(use-package eshell
  :config
  ;; Set the prompt theme to powerline
  (eshell-git-prompt-use-theme 'powerline))
