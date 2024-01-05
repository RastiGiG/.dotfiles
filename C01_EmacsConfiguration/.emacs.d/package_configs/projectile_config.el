;; Use Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; Setup default keybinding for projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Programming")
    (setq projectile-project-search-path
	  '("~/Projects/Programming")))
  (setq projectile-switch-project-action
	#'projectile-dired)
      ;; Add Projectile Functions to User Leader Keys
  (pet/leader-keys
   "p"  '(:ignore t :which-key "projects")
   ;; "pF"  'consult-ripgrep
   "pc"  'projectile-compile-project
   "pd"  'projectile-dired)
  )
