;; Store Location of Account Settings
(setq pet/mail-accounts-config
  	      (concat pet/home-dir
  			      (convert-standard-filename
  			       ".dotfiles-private/MailAccounts.el")))

;; Load mu4e as a Mail Interface for mu
(use-package mu4e
  :straight nil
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; Load org-mode integration
  (require 'mu4e-org)

  ;; Refresh mail using isync/mbsync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync --all")
  (setq mu4e-maildir (concat pet/home-dir "Mail"))

  ;; Sets the standard download directory for attachments
  ;; (default: '~/')
  (setq mu4e-attachment-dir (concat pet/home-dir "Downloads"))

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Load external file with Account information
  (pet/load-file pet/mail-accounts-config)

  ;; Sets the first context (specified in file above)
  ;; to be loaded by default
  ;; (Options: pick-first, ask, ask-if-none, always-ask)
  (setq mu4e-context-policy 'pick-first)

  ;; Don't ask to quit
  (setq mu4e-confirm-quit nil)

  ;; Set Contacts file for Org Contacts interaction
  (setq mu4e-org-contacts-file
  	      (concat pet/org-dir "personal-contacts.org"))

  ;; COMPOSING MAIL

  ;; Don't include oneself in reply by default
  (setq mu4e-compose-dont-reply-to-self t)

  ;; ISO(ish) format date-time stamps in the header list
  ;; default is "%x" (locale appropriate)
  (setq  mu4e-headers-date-format "%Y-%m-%d %H:%M")

  ;; customize the reply-quote-string
  (setq message-citation-line-format
  	      "On %Y-%m-%d %H:%M %Z %N wrote:\n")
  ;; Replace 'message-insert-citation-line' with
  ;; 'message-insert-formatted-citation-line'
  (setq message-citation-line-function
  	      'message-insert-formatted-citation-line)

  ;; HELPER FUNCTIONS

  ;; Function to store header queries to reuse them later
  (defun pet/store-link-to-mu4e-query()
      (interactive)
      (let ((mu4e-org-link-query-in-headers-mode t))
  	(call-interactively 'org-store-link)))

  ;; Functions to automatically call Org Capture Templates on certain actions
  ;; Follow up messages
  (defun pet/capture-mail-follow-up (msg)
      (interactive)
      (call-interactively 'org-store-link)
      (org-capture nil "ef"))
  ;; Read later messages
  (defun pet/capture-mail-read-later (msg)
      (interactive)
      (call-interactively 'org-store-link)
      (org-capture nil "er"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
  			 '("follow up" . pet/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
  			 '("follow up" . pet/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
  			 '("read later" . pet/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
  			 '("read later" . pet/capture-mail-read-later) t)

  (bind-keys
   :map mu4e-headers-mode-map

   ("{" . mu4e-headers-query-prev)             ; differs from built-in
   ("}" . mu4e-headers-query-next)             ; differs from built-in

   ("´" . mu4e-update-mail-and-index)          ; differs from built-in
   ("|" . mu4e-view-pipe)               	     ; does not seem to be built in any longer
   ("." . hydra-mu4e-headers/body))

  ;; Expand personal Keyspace
  (pet/leader-keys
      "m"  '(:ignore t :which-key "Mail")
      "mm" 'mu4e
      "mc" 'mu4e-compose-new
      "ms" 'mu4e-update-mail-and-index))

;; Sent alerts for received
(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (when (boundp 'pet/mu4e-inbox-query-new)
    (setq mu4e-alert-interesting-mail-query
          pet/mu4e-inbox-query-new))

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  ;; Display symbol for received mails on mode line
  (mu4e-alert-enable-mode-line-display)
  ;; Enalbe Notifications
  (mu4e-alert-enable-notifications))
