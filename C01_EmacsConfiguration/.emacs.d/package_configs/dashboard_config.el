;; Add Dashboard to Emacs
(use-package dashboard
  :after (all-the-icons or projectile)
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-week-agenda nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'all-the-icons)  ;; Default is nerd-icons
  (setq dashboard-banner-logo-title "Surveillance creates a prison in the mind")
  ;; use standard emacs logo as banner
  (setq dashboard-startup-banner 'logo)
  ;; Set custom banner
  ;; (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

;; Make Emacsclient start up into dashboard
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
