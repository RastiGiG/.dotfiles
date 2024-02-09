;; Load which-key
;; Loads a more helpful UI Completion buffer
(use-package which-key
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 1
      which-key-side-window-location 'bottom  ;; Bottom is default
      which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      ;; which-key-min-display-lines 6
      which-key-side-window-slot -10
      which-key-side-window-max-height 0.25
      which-key-max-description-length 25
      which-key-allow-imprecise-window-fit t
      which-key-separator " →  "))
