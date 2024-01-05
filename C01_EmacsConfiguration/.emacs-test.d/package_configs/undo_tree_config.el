;; Level up Emacs Undo/Redo
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist `(("." . ,(concat pet/dotfiles-emacsconfig-dir "undo-tree/"))))
  )
