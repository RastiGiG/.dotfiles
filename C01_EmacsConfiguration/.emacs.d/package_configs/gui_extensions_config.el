;; Enable Command Log Mode
(use-package command-log-mode)

;; Visual Fill Column to center text
(use-package visual-fill-column
      :config
      ;; Load fill column when visual line mode
      (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
      ;; Give more space to content, default uses value of fill-column variable
      (setq visual-fill-column-width 140)
      ;; Automatically center text in visual fill column
      (setq-default visual-fill-column-center-text t)

      ;; Add functionality to leader keys
      (pet/leader-keys
  	"tmV"   '(visual-fill-column-mode :which-key "Visual Fill Column")))

;; Add Writeroom Mode
(use-package writeroom-mode
      :config
	      (pet/leader-keys
	"tmW"   '(writeroom :which-key "Writeroom Mode")))

;; Visually Mark Regexp
(use-package visual-regexp)

;; Add Indentation Guideline - usefull for languages like python or yaml
(use-package highlight-indentation
      :hook
      ;; Activate Guideline for programming modes
      (prog-mode . highlight-indentation-mode))
      ;; :config
      ;; (set-face-background 'highlight-indentation-face "#e3e3d3")
      ;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

;; Highlight Hex Strings with matching Colors
(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

;; Add rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; add 256 color support
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; Extend Emacs Emoji capability (apart from Unicode)
(use-package emojify
  ;; if you want to enable emojis globally:
  :hook (after-init . global-emojify-mode))

;; Add Minimap to Emacs
(use-package minimap
  :config
  ;; Set minimap to show on the right
  (setq minimap-window-location 'right)

  ;; Make Minimap available in most text modes
  (add-to-list 'minimap-major-modes 'text-mode)

  ;; Add Minimap to Keyspace for Toggles - Modes
  (pet/leader-keys
      "tmm"   '(minimap-mode :which-key "Minimap Mode")))

;; Load Solaire - Distinguish file buffers with background
(use-package solaire-mode
  :hook (after-init . solaire-global-mode)
  :config
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solair-hl-line-face) solaire-mode-remap-alist))

;; Add Padding to make the GUI pop
(use-package spacious-padding
  :hook
  (after-init . spacious-padding-mode))

;; Automatically adjusts the focused window
(use-package golden-ratio
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))
