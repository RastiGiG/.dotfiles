(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :init
  ;; (add-to-list 'display-buffer-alist
  ;;              '("magit-diff*"
  ;;                display-buffer-in-side-window
  ;;                (side . right)
  ;;                (slot . 3)
  ;;                (window-parameters . ((no-delete-other-windows . t)))
  ;;                (dedicated . t)))

  (add-to-list 'display-buffer-alist
            `(,(make-display-buffer-matcher-function '(magit-mode))
              (display-buffer-reuse-mode-window
               display-buffer-in-direction)
              (mode magit-mode)
              (window . root)
              (window-width . 0.15)
              (direction . left)))
  :config
  ;; Adjust display buffer setting
  ;; (setq magit-display-buffer-function
      ;; 	#'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-display-buffer-function #'display-buffer)

  ;; Add Magit Commands to Leader Key Space
  (pet/leader-keys
      "g"   '(:ignore t :which-key "git")
      "gs"  'magit-status
      "gd"  'magit-diff-unstaged
      "gc"  'magit-branch-or-checkout
      "gl"   '(:ignore t :which-key "log")
      "glc" 'magit-log-current
      "glf" 'magit-log-buffer-file
      "gb"  'magit-branch
      "gP"  'magit-push-current
      "gp"  'magit-pull-branch
      "gf"  'magit-fetch
      "gF"  'magit-fetch-all
      "gr"  'magit-rebase))
