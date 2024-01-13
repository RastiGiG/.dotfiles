;;  ____    _    ____ ___ ____    ___   ___  _     
;; | __ )  / \  / ___|_ _/ ___|  / _ \ / _ \| |    
;; |  _ \ / _ \ \___ \| | |     | | | | | | | |    
;; | |_) / ___ \ ___) | | |___  | |_| | |_| | |___ 
;; |____/_/   \_\____/___\____|  \__\_\\___/|_____|
;;                                                 
;;  ____  _____ _____ _____ ___ _   _  ____ ____  
;; / ___|| ____|_   _|_   _|_ _| \ | |/ ___/ ___| 
;; \___ \|  _|   | |   | |  | ||  \| | |  _\___ \ 
;;  ___) | |___  | |   | |  | || |\  | |_| |___) |
;; |____/|_____| |_|   |_| |___|_| \_|\____|____/ 
;;

;; Setting garbage collection threshold (default is 800)
;; Required for speed and also LSP
(setq gc-cons-threshold (* 50 1000 1000)
 gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
      (lambda ()
        (message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                (float-time
             (time-subtract after-init-time before-init-time)))
            gcs-done)))

;; Silence compiler warnings as they can be pretty
;; disruptive
;;(setq comp-async-report-warnings-errors nil)

;; Tabs as tab-char by default.
;; 'nil' replaces tabs with spaces
;;(setq-default indent-tabs-mode nil)

;; Set the default, fallback tabstop to be 4 spaces
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Set Number of Spaces displayed for a tab stop
(setq-default tab-width 4)
;; ;; Enable Tabs for certain modes
;; (dolist (mode '(text-mode-hook				 
;; 				yaml-mode-hook))
;;   (add-hook mode (lambda () (indent-tabs-mode t))))

;; Show Calendar on StartUp                      
;; (calendar)

;; set date format to %DD-%MM-%YYYY
(setq european-calender-style 't)

;; Remember recently accessed files
(recentf-mode 1)

;; Limit history file to 50 entries to speed up start
(setq history-length 50)
;; Save command and file history
(savehist-mode 1)

;; Remember Cursor Positions on accessed files 
(save-place-mode 1)

;; Avoid Clutter by saving Customization Settings to a different file
(setq custom-file (locate-user-emacs-file "customization_variables.el"))
(load custom-file 'no-error 'no-message)

;; Revert Buffers when Files changed on disk
(global-auto-revert-mode t)

;; Automatically revert Dired (and similar) Buffers without confirmation
(setq global-auto-revert-non-file-buffers t)

; Load newer file version when possible
(setq load-prefer-newer t)

; Type ’y’ and ’n’ instead of ’yes’ or ’no’
(setq use-short-answers t)

;; Make Selections behave as expected
(delete-selection-mode 1)    ;; You can select text and delete it by typing.

;; Turns on automatic parens pairing
(electric-pair-mode 1)

;; Replace Quotation Characters in Comments and Textblocks
(electric-quote-mode 1)
(setq electric-quote-comment t)
(setq electric-quote-string t)
(setq electric-quote-paragraph t)

;; Places newline characters automatically
(electric-layout-mode 1)

(setq explicit-shell-file-name "bash")
;;(setq explicit-zsh-args '())
;; Regexp to use when searching for last prompt
(setq term-prompt-regexp
      "^[^#$%>\\n]*[#$%>] *")

;; Don’t spawn buffers left and right
(setq dictionary-use-single-buffer t);

;; Display dictionary as a sidebar left
;; mandatory, as the dictionary misbehaves!
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
   '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 50)))

;; Make Emacs use the local server
(setq dictionary-server "localhost")

;; Setup World Clock list
;; If not set, zoneinfo-style-world-list is used
(setq world-clock-list
  '(("Etc/UTC" "UTC")
    ("Europe/Berlin" "Berlin")
    ("Europe/Paris" "Paris")
    ("Europe/London" "London")
    ("Europe/Athens" "Athens")
    ("America/New_York" "New York")
    ("America/Los_Angeles" "Seattle")
    ("America/Mexico_City" "Mexico City")
    ("Asia/Shanghai" "Shanghai")
    ("Asia/Calcutta" "Bangalore")
    ("Asia/Tokyo" "Tokyo")
    ("Pacific/Auckland" "Auckland"))
  )

;; Adjust how time is displayed
(setq display-time-world-time-format
  "%A, %d %B %Y %H:%M %p %Z")

;; Bind Ace Window Control
(global-set-key (kbd "M-o") 'ace-window)
