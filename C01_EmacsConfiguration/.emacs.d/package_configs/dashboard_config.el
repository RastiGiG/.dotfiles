;; Function to insert vocabulary
(defun pet/dashboard-insert-vocabulary (list-size)
  (dashboard-insert-heading "Word of the Day:"
  						      nil
  						      (all-the-icons-faicon "newspaper-o"
  												:height 1.2
  												:v-adjust 0.0
  												:face 'dashboard-heading))
  (insert "\n")
  (let ((random-line nil)
  	      (lines nil))
      (with-temp-buffer
  	(insert-file-contents (concat user-emacs-directory "words"))
  	(goto-char (point-min))
  	(setq lines (split-string (buffer-string) "\n" t))
  	(setq random-line (nth (random (length lines)) lines))
  	(setq random-line (string-join (split-string random-line) " ")))
      (insert "    " random-line)))

;; Function to insert ledger bal
(defun pet/dashboard-ledger-monthly-balances (list-size)
  "Return the monthly balance by using ledger"
  (interactive)
  (dashboard-insert-heading "Monthly Balance:"
  						      nil
  						      (all-the-icons-faicon "money"
  												:height 1.2
  												:v-adjust 0.0
  												:face 'dashboard-heading))
  (insert "\n")
  (let* ((current-month (format-time-string "%Y/%m"))
  	       (cmd (format "ledger bal --flat --monthly --period %s %s "
  					current-month
  					(mapconcat 'identity pet/ledger-categories " "))))
      (insert (shell-command-to-string cmd))))

;; Add Dashboard to Emacs
(use-package dashboard
  :after (all-the-icons)
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
  (setq dashboard-center-content t) ;; set to 'nil' for uncentered content
  (setq dashboard-show-shortcuts t)  ;; show shortcuts
  (setq dashboard-projects-backend 'projectile)  ;; default ’projectile’
  ;; (dashboard-agenda-sort-strategy '(priority-down))
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
  						(registers . 3)
  						;; (vocabulary)  ;; Needs vocabulary file
  						(monthly-balance)))
  (setq dashboard-item-generators '((monthly-balance . pet/dashboard-ledger-monthly-balances)
  								      (vocabulary . pet/dashboard-insert-vocabulary)
  								      (recents . dashboard-insert-recents)
  								      (bookmarks . dashboard-insert-bookmarks)
  								      (projects . dashboard-insert-projects)
  								      (agenda . dashboard-insert-agenda)
  								      (registers . dashboard-insert-registers)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  ;; Make Emacsclient start up into dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
