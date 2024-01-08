;; Use Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :demand t
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Programming")
    (setq projectile-project-search-path
          '("~/Projects/Programming")))
  (setq projectile-switch-project-action
        #'projectile-dired)

  ;; Activate Projectile
  (projectile-mode)
  ;; Setup default keybinding for projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  ;; Specify a list of directories and files to ignore by projectile grep
  ;; Can interpret regular expressions
  (cl-loop for file in
           '()
           do
           (add-to-list 'projectile-globally-ignored-files file))
  (cl-loop for directory in
           '("backup")
           do
           (add-to-list 'projectile-globally-ignored-directories directory))

  ;; Add Projectile Functions to User Leader Keys
  (pet/leader-keys
    "p"  '(:ignore t :which-key "projects")
    ;; "pF"  'consult-ripgrep
    "pc"  'projectile-compile-project
    "pd"  'projectile-dired))
