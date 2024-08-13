;; Load PDF Tools to replace DocView
(use-package pdf-tools
  :straight nil  ;; loaded with guix package manage
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :init
  ;; Install PDF Tools in all buffers
  (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf")

  ;; Disable linum mode in PDF Tools
  (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
  (pet/leader-keys
    ;; Toggles - Modes
    "tmp"   '(pdf-view-mode :which-key "PDF View Mode")))

;; Enable bookmarking in pdf-view buffers
(use-package saveplace-pdf-view
  :after pdf-tools
  :config
  (save-place-mode 1))
