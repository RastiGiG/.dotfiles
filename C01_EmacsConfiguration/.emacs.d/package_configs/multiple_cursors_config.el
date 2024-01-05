;; Multiple cusors are a must. Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "C-S-c C-S-c")
                  'mc/edit-lines)
  (global-set-key (kbd "C-<")
                  'mc/mark-previous-like-this)
  (global-set-key (kbd "C->")
                  'mc/mark-next-like-this)
  (global-set-key (kbd "C-c M-<")
                  'mc/mark-all-like-this)
  (global-set-key (kbd "s-D")
                  'mc/mark-all-dwim)
  (define-key mc/keymap (kbd
                         "<return>") nil))
