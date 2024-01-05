;; Load PDF Tools to replace DocView
(use-package pdf-tools
      :config
      ;; Install PDF Tools in all buffers
      (pdf-tools-install)
      ;; Disable linum mode in PDF Tools
      ;; (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))

      (pet/leader-keys
	;; Toggles - Modes
	"tmp"   '(pdf-view-mode :which-key "PDF View Mode")
      ))
