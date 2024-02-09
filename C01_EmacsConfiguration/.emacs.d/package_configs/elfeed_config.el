;; Load Elfeed
(use-package elfeed
  :bind (("C-c f" . elfeed)
  	       :map elfeed-search-mode-map
  	       ("n" . (lambda () (interactive)
  				(next-line) (call-interactively
  					 'elfeed-search-show-entry)))
  	       ("p" . (lambda () (interactive)
  				(previous-line) (call-interactively
  						 'elfeed-search-show-entry)))
  	       ("m" . (lambda () (interactive)
  				(apply 'elfeed-search-toggle-all '(star))))
  	       ("g" . elfeed-update)
  	       ("G" . elfeed-search-update--force))
  	       ;;:map elfeed-show-mode-map
  	       ;;("w" . elfeed-show-yank))
  :config
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-remain-on-entry t)
  ;; Various Necessary/Helpful Settings
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory
  	      (concat pet/dotfiles-emacsconfig-dir "elfeed/"))
  (setq elfeed-enclosure-default-dir
  	      "~/Downloads/")
  (setq elfeed-search-filter
  	      "@4-months-ago +unread")
  (setq elfeed-sort-order 'descending)
  (setq elfeed-search-clipboard-type 'CLIPBOARD)
  (setq elfeed-search-title-max-width 150)
  (setq elfeed-search-title-min-width 30)
  (setq elfeed-search-trailing-width 25)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-show-unique-buffers t)
  (setq elfeed-search-date-format
  	      '("%F %R" 16 :left))

  ;; Snippet for periodic update for feeds
  ;; (add-to-list 'elfeed-update-hooks 'elfeed-update)
  ;; (run-with-timer 0 (* 60 60 4) 'elfeed-update)

  ;; Load Feeds and Feed Settings
  (pet/load-file (concat pet/dotfiles-emacsconfig-dir
  					       "EmacsRSSFeed.el")))

;; Load Elfeed Score
(use-package elfeed-score
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "="
                elfeed-score-map))
  (setq elfeed-search-print-entry-function
        #'elfeed-score-print-entry)
  (setq elfeed-score-serde-score-file
        (concat pet/dotfiles-emacsconfig-dir
         "elfeed.score"))
  (setq elfeed-score-rule-stats-file
        (concat pet/dotfiles-emacsconfig-dir
                "elfeed.stats")))

;; Configure Elfeed with Org Mode
(use-package elfeed-org
  :config
  ;; Default Elfeed config can be found under "~/.emacs.d/elfeed.org"
  (setq rmh-elfeed-org-files (list "~/Org/elfeed.org"))
  ;; Automatically set ignore tag for feeds with errors
  (setq rmh-elfeed-org-auto-ignore-invalid-feeds t)
  ;; Hook elfeed-org up to elfeed
  (elfeed-org))

;; Add Dashboard to Elfeed
(straight-use-package
  '(elfeed-dashboard
    :type git
    :host github
    :repo "Manoj321/elfeed-dashboard"))

;; Configure Dashboard
(setq elfeed-dashboard-file "~/Org/elfeed-dashboard.org")
(advice-add 'elfeed-search-quit-window
            :after #'elfeed-dashboard-update-links)
