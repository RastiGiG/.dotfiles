;;   _____ __  __    _    ____ ____  
;;  | ____|  \/  |  / \  / ___/ ___| 
;;  |  _| | |\/| | / _ \| |   \___ \ 
;;  | |___| |  | |/ ___ \ |___ ___) |
;;  |_____|_|  |_/_/   \_\____|____/ 
;;                                   

;; NOTE: init.el is generated from EmacsTestConfig.org.
 ;; Please change your settings in that file
 ;; using Emacs and init.el will be generated automatically!

;; Setting Variables
;; for better customization and readability

;; Save Dotfiles Dirs for later use
(setq pet/dotfiles-dir
      (expand-file-name
       (convert-standard-filename
       "~/.dotfiles/")))
(setq pet/dotfiles-emacsconfig-dir
        (concat pet/dotfiles-dir
         "C01_EmacsConfiguration/"))

;; Adjust font size to match your system
(defvar pet/default-font-size 140)
(defvar pet/default-variable-font-size 120)

;; bootstrap script to install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Make sure to always install packages (pendant to use-package-always-ensure)
(setq straight-use-package-by-default t)

;; This is set just to be able to lookup packages
;; It's not required since we use straight anyway
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; A few basic settings

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Start Emacs in Fullscreen mode and set transparancy
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Set default Encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Set up the visible bell
(setq visible-bell t)

;; Set Column Numbers
(column-number-mode)
;; Set Line Numbers Globally
(global-display-line-numbers-mode t)
;; Set Visual Line Mode for text modes only
;; Preferred over global-visual-line-mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka" :height pet/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height pet/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height pet/default-font-size :weight 'regular)

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

;; Silence compiler warnings as they can be pretty disruptive
;;(setq comp-async-report-warnings-errors nil)

;; Show Calendar on StartUp                      
;; (calendar)

;; set date format to %DD-%MM-%YYYY
(setq european-calender-style 't)

; Setup file containing global macros
(load-file
 (concat pet/dotfiles-emacsconfig-dir
         "macros/global.macs")) 

;; Set of keybindings for defined macros
;; Make sure to have a definition of the macro in your /macros folder
(global-set-key "\C-x\C-kT" 'transpose-names)

;; Set Location for bookmarks file/s
(setq bookmark-default-file
      (concat pet/dotfiles-emacsconfig-dir
              "bookmarks"))

;; Activate Abbrev Mode by default
(setq-default abbrev-mode t)

;; Set Location and Name of Abbrev file
(setq abbrev-file-name
      (concat pet/dotfiles-emacsconfig-dir
              "abbrev_defs"))

;; Save Abbrevs when saving Files
(setq save-abbrevs t)

;; Setup World Clock list
;; If not set, zoneinfo-style-world-list is used
(setq world-clock-list
      '(("Europe/Berlin" "Berlin")
	("Europe/Paris" "Paris")
	("Europe/London" "London")
	("America/New_York" "New York")
	("America/Los_Angeles" "Seattle")
	("Asia/Calcutta" "Bangalore")
	("Asia/Tokyo" "Tokyo"))
      )

;; Adjust how time is displayed
(setq display-time-world-time-format "%a, %d %b %H:%M %p %Z")

(setq pet/yasnippet-dir
      (concat pet/dotfiles-emacsconfig-dir
              "snippets"))

;; Yasnippets
(use-package yasnippet
  :config
  ;; Set Yasnippet dir
  (setq yas-snippet-dirs '(pet/yasnippet-dir))

  ;; Activate Yasnippets globally
  (yas-global-mode 1)

  ;; Enable snippets being shared between modes
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode
               'fundamental-mode))))

;; Multiple cusors are a must. Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "C-S-c C-S-c")
                  'mc/edit-lines)
  (global-set-key (kbd "C-<")
                  'mc/mark-previous-like-this)
  (global-set-key (kbd "C->")
                  'mc/mark-next-like-this)
  (global-set-key (kbd "C-c M-<")
                  'mc/mark-all-like-this)
  (global-set-key (kbd "s-D")
                  'mc/mark-all-dwim)
  (define-key mc/keymap (kbd
                         "<return>") nil))

;; Extend Emacs Emoji capability (apart from Unicode)
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Setup general for easier key config
(use-package general
  :config
  (general-create-definer pet/leader-keys
  :prefix "C-."
  :global-prefix "C-.")

  (pet/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme
	  :which-key "choose theme")
   "tw" 'whitespace-mode
   "tc" 'world-clock
   "l"  '(:ignore t :which-key "layout")
  ))

;; Add Dashboard to Emacs
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Surveillance creates a prison in the mind")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
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

;; Enable Command Log Mode
(use-package command-log-mode)

;; Load Doom Themes
;; DISABLED TO DISTINGUISH TEST CONFIG
(use-package doom-themes
  ;; :init (load-theme 'doom-dracula t)
  )

;; Use all-the-icons
;;required for doom modeling
(use-package all-the-icons)

;; Load doom modeline
(use-package doom-modeline
  ;; Activate Doom Modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Load which-key
;; Loads a more helpful UI Completion buffer 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Tab Bar Mode Setting

;; Set new tab to scratch buffer
(setq tab-bar-new-tab-choice "*scratch*")
;; right is default -
;; change if you dont like that
;; (tab-bar-new-tab-to right)                

;; Set the name of the tab to
;; match the current buffer
;; (setq tab-bar-tab-name-function
;;       tab-bar-current-tab-name)

  ;; Keyboard Rules
  ;; Remove Tab Bar Buttons
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        ;; tab-bar-button-relief               ;; controls outline of buttons
        ;; tab-bar-face tab-bar-tab            ;; configure tab face (bgcolor etc.)
        )

  ;; tab bar is not automatically shown
  ;; (set 1 to enable)
  (setq tab-bar-show nil)                      

  ;; Helper function to get only the name
  ;; of current tab
  (defun pet/current-tab-name ()
    (alist-get 'name (tab-bar--current-tab)))

;; Load Ivy Completion Framework
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Add Counsel for customized find files etc..
(use-package counsel
  :after ivy
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  ;; Add Counsel function to leader key space
  (pet/leader-keys
    "r"   '(ivy-resume :which-key "ivy resume")
    "f"   '(:ignore t :which-key "files")
    "ff"  '(counsel-find-file :which-key "open file")
    "C-f" 'counsel-find-file
    "fr"  '(counsel-recentf :which-key "recent files")
    "fR"  '(revert-buffer :which-key "revert file")
    "fj"  '(counsel-file-jump :which-key "jump to file")))

  ;; Ivy-Rich: Add Descriptions alongside M-x commands
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Add Prescient for spooky Emacs Memory (history)
(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

;; Enable Prescient in Ivy
(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

;; Use Helpful to get a better help buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Add Perspective to use sets of 
(use-package perspective
  :demand t
  ;; Setup Keybindings
  ;; :bind (("C-M-k" . persp-switch)
  ;; 	   ("C-M-n" . persp-next)
  ;; 	   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  (persp-state-default-file
   (concat pet/dotfiles-emacsconfig-dir
	   "perspective/default-state"))
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode))

  ;; Add Perspective Functions to User Leader Keys
  (pet/leader-keys
   "P"  '(:ignore t :which-key "Perspectives")
   "Pn"  'persp-next  
   "Ps"  'persp-switch-to-buffer*
   "Pk"  'persp-kill-buffer*
  )
  )

(use-package treemacs
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-<f8>" . treemacs-select-window))
  :config
  ;; ensure that treemacs-buffer is
  ;; ignored when switching windows 
  (setq treemacs-is-never-other-window t)

  ;; Add shortcut for treemacs to
  ;; personal keyspace
  (pet/leader-keys
   "lt"  '(:ignore t :which-key "treemacs")
   "ltt" 'treemacs
   "ltw" 'treemacs-select-window)
  )

(use-package elfeed
  :bind (("C-c f" . elfeed)
	 :map elfeed-search-mode-map
	 ("n" . (lambda () (interactive)
		  (next-line)
		  (call-interactively
		   'elfeed-search-show-entry)))
	 ("p" . (lambda () (interactive)
		  (previous-line)
		  (call-interactively
		   'elfeed-search-show-entry)))
	 ("m" . (lambda () (interactive)
		  (apply
		   'elfeed-search-toggle-all
		   '(star))))
	 ("g" . elfeed-update)
	 ("G" . elfeed-search-update--force)
	 ;;:map elfeed-show-mode-map
	 ;;("w" . elfeed-show-yank))
	 )
  :config
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-remain-on-entry t)
  ;; Various Necessary/Helpful Settings
  (setq elfeed-use-curl t)
  (setq elfeed-curl-max-connections 10)
  (setq elfeed-db-directory
	(concat pet/dotfiles-emacsconfig-dir
		"elfeed/"))
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
;; (3 mins since Emacs start, then every
;; half hour)
  (run-at-time 180 1800
	       (lambda ()
		 (unless elfeed-waiting
		   (elfeed-update))))
  )
;; Load Feeds and Feed Settings  
(load (concat pet/dotfiles-emacsconfig-dir
	      "EmacsRSSFeed.el"))

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

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; default for auto-save-list-file-prefix is "~/.emacs.d/auto-save-list/.saves~"
;; this moves it to a more centralized location (tmp)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Configuring Dired
(use-package dired
  :straight nil
  ;; Defer loading of dired config til one of the commands is used
  :commands (dired dired-jump)
  ;; The prefixes are arguments given to "ls" by dired
  :custom ((dired-listing-switches
            "-aghlv --group-directories-first"))
  :bind (("C-x C-j" . dired-jump))
    )

;; Adds icons to files and directories in dired           
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

;; Use dired-open to launch external apps 
(use-package dired-open)
;; open .png files in 'sxiv' and .mp4 files to open in 'mpv'
;; open .pdf in 'zahtura'
(setq dired-open-extensions '(("gif" . "sxiv")
			      ("jpg" . "sxiv")
			      ("png" . "sxiv")
			      ("mkv" . "mpv")
			      ("mp4" . "mpv")
			      ("pdf" . "zathura")))

;; Helper Functions for Org
(defun pet/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.02)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute
     (car face)
     nil
     :font "Cantarell"
     :weight 'regular
     :height (cdr face)))

  ;; Ensure that anything that should be
  ;; fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil
                      :foreground nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face
                                 fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil
                      :inherit 'fixed-pitch))

;; Setting Up Org Mode
(use-package org
  :bind (("C-c l" . org-store-link))
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory
	(convert-standard-filename "~/Org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; setup inline previewing of latex fragments
  (setq org-latex-create-formula-image-program
	'imagemagick)

  (setq org-agenda-files
	'("~/Org/journal"
	  "~/Org/personal-tasks.org"
	  "~/Org/personal-mail.org"
	  "~/Org/personal-chores.org"))

  ;; Set Org Clock Sound File
  (setq org-clock-sound "/home/sebastian/Org/sounds/Rush.wav")
  )

;; Setup Org Superstar
(use-package org-superstar
  :after org)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Org Capture helper Function
(defun pet/create-documents-file ()
  "Create an org file in ~/Org/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name
     (format "%s.org" name))))

;; Org-Capture
(use-package org-capture
  :straight nil
  :config
   (setq org-capture-templates
         ;; Acronym captures
         `(("a" "Acronyms")

           ("ag" "General Acronyms")
           ("agg" "General Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("agt" "General Acronyms - Terminology" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "Terminology")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("as" "Scientific Acronyms")
           ("ase" "Scientific Acronyms - Economy" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Economy")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asg" "Scientific Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asm" "Scientific Acronyms - Maths" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Maths")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asp" "Scientific Acronyms - Physics" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Physics")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("ai" "IT related Acronyms")
           ("aic" "IT related Acronyms - Encryption" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encryption")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aim" "IT related Acronyms - Mail" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Mail")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aie" "IT related Acronyms - Emacs" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Emacs")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aig" "IT related Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aii" "IT related Acronyms - Internet" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Internet")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ail" "IT related Acronyms - LaTeX" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "LaTeX")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ain" "IT related Acronyms - Networks" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Networks")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aip" "IT related Acronyms - Programming" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Programming")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aiu" "IT related Acronyms - Encoding" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encoding")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")  


           ;; Documents
           ("d" "Documents")
           ("dl" "Letter")
           ("dlf" "Letter Form" plain (file pet/create-documents-file)
            "%[~/.dotfiles/00_OrgFiles/Templates/Capture-LetterTemp.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )
           ("dlh" "Letter Home" plain (file pet/create-documents-file)
            "%[~/Templates/X1_Emacs_Templates/Capture-LetterTemp-Filled-Home-Real.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )


           ;; Email captures
           ("e" "Email")
           ("em" "Make email note" entry
            (file+headline "~/Org/personal-tasks.org" "Mail correspondence")
            ,(concat "* TODO [#A] %:subject :mail:\n"
                     "SCHEDULED: %t\n:"
                     "PROPERTIES:\n:CONTEXT: %a\n:END:\n\n"
                     "%i%?"))
           ("ef" "Follow Up" entry (file+olp "~/Org/personal-mail.org" "Follow Up")
            "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)
           ("er" "Read Later" entry (file+olp "~/Org/personal-mail.org" "Read Later")
            "* TODO Read %:subject %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)


           ;; Journal captures
           ("j" "Journal Entries")
           ("jj" "Journal" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
            ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
            :clock-in :clock-resume
            :empty-lines 1)
           ("jm" "Meeting" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
            :clock-in :clock-resume
            :empty-lines 1)


           ;; Checklist captures
           ("l" "Lists")

           ("ls" "Shopping List")
           ("lsp" "Permanent & Long Lasting")
           ("lspw" "Living" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =")
            "%^{Itemname}")
           ("lspd" "Technology" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Technik =")
            "%^{Itemname}")
           ("lspdc" "Computer" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Computer =")
            "%^{Itemname}")
           ("lspdh" "Appliances" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Haushaltsgeräte =")
            "%^{Itemname}")
           ("lspt" "Transport" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Transport =")
            "%^{Itemname}")
           ("lsv" "Consumables & Usables")
           ("lsvb" "Office Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Büromaterial =")
            "%^{Itemname}")
           ("lsvl" "Groceries" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Lebensmittel =")
            "%^{Itemname}")
           ("lsvr" "Cleaning Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Reinigungs- und Pflegemittel =")
            "%^{Itemname}")

           ("ll" "Literature")
           ("lls" "Scientific Literature")
           ("llsb" "Biology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsc" "Chemistry" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Chemie ==") "[ ] %^{Author} - %^{Title}")
           ("llse" "Politics, Economy and Ecology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Politik, Ökonomie und Ökologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsg" "History" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== History ==") "[ ] %^{Author} - %^{Title}")
           ("llsh" "Medicine and Health" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Medizin ==") "[ ] %^{Author} - %^{Title}")
           ("llsi" "IT" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Informatik, Data-Science und AI ==") "[ ] %^{Author} - %^{Title}")
           ("llsm" "Maths" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Mathematik ==") "[ ] %^{Author} - %^{Title}")
           ("llsp" "Physics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Physik ==") "[ ] %^{Author} - %^{Title}")
           ("llss" "Philosophy and Sociology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llst" "Technology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Technik ==") "[ ] %^{Author} - %^{Title}")
           ("llsl" "Languages" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Sprachen ==") "[ ] %^{Author} - %^{Title}")
           ("llsz" "Psychology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Psychologie ==") "[ ] %^{Author} - %^{Title}")

           ("llr" "Novels" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =") "[ ] %^{Author} - %^{Title}")
           ("llrk" "Classics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =" "== Klassiker ==") "[ ] %^{Author} - %^{Title}")


           ("lm" "Music")
           ("lmd" "Downlaodable" checkitem
            (file+olp "~/Org/lists-music.org" "TODO Musik zum Downloaden")
            "[ ] %^{Interpret} - %^{Title}")

           ("q" "Quotes")
           ("qt" "Talks" entry
            (file+olp "~/Org/personal-quotes.org" "Reden und Interviews")
            "* %^{Originator} \n %?")
           ("ql" "Literature" entry
            (file+olp "~/Org/personal-quotes.org" "Literatur")
            "* %^{Originator} \n %?")


           ("t" "Tasks / Projects")
           ("tt" "TODO Task" entry (file+olp "~/Org/personal-tasks.org" "Inbox")
            "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)  
           ("tb" "Basic task for future review" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Title}\n"
                     ":PROPERTIES:\n"
                     ":CAPTURED: %U\n"
                     ":END:\n\n"
                     "%i%l"))
           ("ts" "Task with a due date (scheduled)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "SCHEDULED: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))
           ("td" "Task with a due date (deadline)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "DEADLINE: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))


           ("w" "Workflows")
           ("we" "Checking Email" entry (file+olp+datetree "~/Org/journal/Journal.org")
            "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))


  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")
                (in-mode . "mu4e-headers-mode")))))
  :bind
  ("C-c c" . org-capture))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
                \\usepackage{hyperref}
                \\usepackage{babel}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
           '("org-plain-scrlttr2-german"
             "\\documentclass[a4paper, 
              parskip=half,%
              fromalign=right, 
              fromrule=false, 
              11pt, ngerman]{scrlttr2}
              \\usepackage{hyperref}
              \\usepackage{babel}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Bigger LaTeX Previews
(plist-put org-format-latex-options :scale 1.5)
;; Load language packages for pdflatex of lualatex / xelatex compilers
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "babel" t ("pdflatex")))
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
)

(use-package ob-ipython)

;; (require-package 'ob-ipython)

;; enable/disable languages for org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)    ;; Elisp
    (lisp . t)          ;; Lisp
    (clojure . t)       ;; Clojure     
    (scheme . t)        ;; Scheme
    (python . t)        ;; Python
    (ipython . t)       ;; IPython
    ;;  the following two require ob-c
    ;; (c . t)             ;; C 
    ;; (cpp . t)           ;; C++

    (R . t)             ;; R
    (shell . t)         ;; Command Line Programs 
    (latex . t)         ;; LaTeX  
    (sql . t)           ;; SQL
    (sqlite . t)        ;; SQLite
    (octave . t)        ;; Octave
    (gnuplot . t)       ;; Gnuplot
    (awk . t)           ;; awk
    (sed . t)))         ;; GNUsed

;; Add conf-unix to be recognized
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

;; Setup Source Block Templates
(cl-loop for block in
         '(("sh" . "src shell")
           ("se" . "src emacs-lisp")
           ("sp" . "src python")
           ("sq" . "src sql")
           ("so" . "src octave")
           ("si" . "src ipython :session :async :exports both :results raw drawer")
           ;; This is an alternative Block
           ;; For IPython
           ;; ("si" . "src ipython :session :async :results output")
           )
         do
         (add-to-list
          'org-structure-template-alist block))

(defun pet/create-documents-file ()
  "Create an org file in ~/notes/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name
     (format "%s.org" name))))

;; Org-Capture
(use-package org-capture
  :straight nil
  :config
   (setq org-capture-templates
         ;; Acronym captures
         `(("a" "Acronyms")

           ("ag" "General Acronyms")
           ("agg" "General Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("agt" "General Acronyms - Terminology" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "Terminology")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("as" "Scientific Acronyms")
           ("ase" "Scientific Acronyms - Economy" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Economy")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asg" "Scientific Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asm" "Scientific Acronyms - Maths" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Maths")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asp" "Scientific Acronyms - Physics" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Physics")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("ai" "IT related Acronyms")
           ("aic" "IT related Acronyms - Encryption" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encryption")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aim" "IT related Acronyms - Mail" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Mail")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aie" "IT related Acronyms - Emacs" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Emacs")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aig" "IT related Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aii" "IT related Acronyms - Internet" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Internet")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ail" "IT related Acronyms - LaTeX" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "LaTeX")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ain" "IT related Acronyms - Networks" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Networks")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aip" "IT related Acronyms - Programming" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Programming")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aiu" "IT related Acronyms - Encoding" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encoding")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")  


           ;; Documents
           ("d" "Documents")
           ("dl" "Letter")
           ("dlf" "Letter Form" plain (file efs/create-documents-file)
            "%[~/.dotfiles/00_OrgFiles/Templates/Capture-LetterTemp.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )
           ("dlh" "Letter Home" plain (file efs/create-documents-file)
            "%[~/Templates/X1_Emacs_Templates/Capture-LetterTemp-Filled-Home-Real.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )


           ;; Email captures
           ("e" "Email")
           ("em" "Make email note" entry
            (file+headline "~/Org/personal-tasks.org" "Mail correspondence")
            ,(concat "* TODO [#A] %:subject :mail:\n"
                     "SCHEDULED: %t\n:"
                     "PROPERTIES:\n:CONTEXT: %a\n:END:\n\n"
                     "%i%?"))
           ("ef" "Follow Up" entry (file+olp "~/Org/personal-mail.org" "Follow Up")
            "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)
           ("er" "Read Later" entry (file+olp "~/Org/personal-mail.org" "Read Later")
            "* TODO Read %:subject %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)


           ;; Journal captures
           ("j" "Journal Entries")
           ("jj" "Journal" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
            ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
            :clock-in :clock-resume
            :empty-lines 1)
           ("jm" "Meeting" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
            :clock-in :clock-resume
            :empty-lines 1)


           ;; Checklist captures
           ("l" "Lists")

           ("ls" "Shopping List")
           ("lsp" "Permanent & Long Lasting")
           ("lspw" "Living" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =")
            "%^{Itemname}")
           ("lspd" "Technology" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Technik =")
            "%^{Itemname}")
           ("lspdc" "Computer" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Computer =")
            "%^{Itemname}")
           ("lspdh" "Appliances" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Haushaltsgeräte =")
            "%^{Itemname}")
           ("lspt" "Transport" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Transport =")
            "%^{Itemname}")
           ("lsv" "Consumables & Usables")
           ("lsvb" "Office Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Büromaterial =")
            "%^{Itemname}")
           ("lsvl" "Groceries" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Lebensmittel =")
            "%^{Itemname}")
           ("lsvr" "Cleaning Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Reinigungs- und Pflegemittel =")
            "%^{Itemname}")

           ("ll" "Literature")
           ("lls" "Scientific Literature")
           ("llsb" "Biology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsc" "Chemistry" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Chemie ==") "[ ] %^{Author} - %^{Title}")
           ("llse" "Politics, Economy and Ecology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Politik, Ökonomie und Ökologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsg" "History" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== History ==") "[ ] %^{Author} - %^{Title}")
           ("llsh" "Medicine and Health" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Medizin ==") "[ ] %^{Author} - %^{Title}")
           ("llsi" "IT" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Informatik, Data-Science und AI ==") "[ ] %^{Author} - %^{Title}")
           ("llsm" "Maths" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Mathematik ==") "[ ] %^{Author} - %^{Title}")
           ("llsp" "Physics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Physik ==") "[ ] %^{Author} - %^{Title}")
           ("llss" "Philosophy and Sociology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llst" "Technology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Technik ==") "[ ] %^{Author} - %^{Title}")
           ("llsl" "Languages" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Sprachen ==") "[ ] %^{Author} - %^{Title}")
           ("llsz" "Psychology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Psychologie ==") "[ ] %^{Author} - %^{Title}")

           ("llr" "Novels" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =") "[ ] %^{Author} - %^{Title}")
           ("llrk" "Classics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =" "== Klassiker ==") "[ ] %^{Author} - %^{Title}")


           ("lm" "Music")
           ("lmd" "Downlaodable" checkitem
            (file+olp "~/Org/lists-music.org" "TODO Musik zum Downloaden")
            "[ ] %^{Interpret} - %^{Title}")

           ("q" "Quotes")
           ("qt" "Talks" entry
            (file+olp "~/Org/personal-quotes.org" "Reden und Interviews")
            "* %^{Originator} \n %?")
           ("ql" "Literature" entry
            (file+olp "~/Org/personal-quotes.org" "Literatur")
            "* %^{Originator} \n %?")


           ("t" "Tasks / Projects")
           ("tt" "TODO Task" entry (file+olp "~/Org/personal-tasks.org" "Inbox")
            "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)  
           ("tb" "Basic task for future review" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Title}\n"
                     ":PROPERTIES:\n"
                     ":CAPTURED: %U\n"
                     ":END:\n\n"
                     "%i%l"))
           ("ts" "Task with a due date (scheduled)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "SCHEDULED: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))
           ("td" "Task with a due date (deadline)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "DEADLINE: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))


           ("w" "Workflows")
           ("we" "Checking Email" entry (file+olp+datetree "~/Org/journal/Journal.org")
            "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))


  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")
                (in-mode . "mu4e-headers-mode")))))
  :bind
  ("C-c c" . org-capture))

;; Org Roam is very handy to create a 'second brain'
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org")
  (org-roam-dailies-directory "journal/")

  (org-roam-completion-everywhere t)

  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 ("C-M-i"    . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config

  ;; org roam capture templates
  (setq org-roam-capture-templates
	`(("d" "default" plain
	  "%?"
	  :if-new (file+head
		   "%<%Y%m%d%H%M%S>-${slug}.org"
		   "#+TITLE: ${title}\n#+DATE: %U\n")
	  :unnarrowed t)
	  ("l" "programming language" plain
	   "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
	   :if-new (file+head "${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unnarrowed t)  
	  ("b" "book notes" plain (file "~/.dotfiles/00_OrgFiles/Templates/RoamCapture-BookNoteTemp.org")
	   :if-new (file+head "${slug}.org"
			      "#+TITLE: ${title}\n")
	   :unnarrowed t)
	  ("p" "project" plain
	   "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	   :if-new (file+head "${slug}.org"
			      "#+TITLE: ${title}\n#+filetags: Project")
	   :unnarrowed t)
	  ))


  ;; dailies capture template
  (setq org-roam-dailies-capture-templates
	`(("d" "default" entry "* %<%I:%M %p>: %?"
	   :if-new (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))

  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;; Helper Function to insert org note immediately
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
	(org-roam-capture-templates
	 (list (append (car org-roam-capture-templates)

		       '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;;
  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui"
	   :branch "main" :files ("*.el" "out"))
    :after org-roam
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save t
	  org-roam-ui-open-on-start t))

(use-package org-drill
  :config
  (progn
    (add-to-list 'org-modules 'org-drill)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-hint-separator "||")
    (setq org-drill-left-cloze-delimiter "<[")
    (setq org-drill-right-cloze-delimiter "]>")
    (setq org-drill-learn-fraction 1.0))
  )

;; Add rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Customize highlighting of matching parenthesis
(use-package paren
:config
(set-face-attribute
 'show-paren-match-expression nil :background "#363e4a")
(show-paren-mode 1))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  ;;(setq explicit-zsh-args '())
  ;; Regexp to use when searching for last prompt
  (setq term-prompt-regexp
        "^[^#$%>\\n]*[#$%>] *"))

;; add 256 color support
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;; uncomment this line if you want to use zsh
  ;; (setq vterm-shell "zsh")
  ;; set maximum lines of output to be stored in RAM
  (setq vterm-max-scrollback 10000))

;; adds git related prompt elements to eshell
(use-package eshell-git-prompt)

(use-package eshell

  :config
  ;; Set the prompt theme to powerline
  (eshell-git-prompt-use-theme 'powerline))

;; Use Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
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
   "pf"  'counsel-projectile-find-file
   "ps"  'counsel-projectile-switch-project
   "pF"  'counsel-projectile-rg
   ;; "pF"  'consult-ripgrep
   "pp"  'counsel-projectile
   "pc"  'projectile-compile-project
   "pd"  'projectile-dired)
  )

;; Projectile Counsel Integration
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Add Language Server Support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
	      ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil)
  :config (lsp-enable-which-key-integration t))

;; Add Lsp Functions to Leader Keys
(pet/leader-keys
 "tl"  '(:ignore t :which-key "lsp")
 "tld" 'xref-find-definitions
 "tlr" 'xref-find-references
 "tln" 'lsp-ui-find-next-reference
 "tlp" 'lsp-ui-find-prev-reference
 "tls" 'counsel-imenu
 "tle" 'lsp-ui-flycheck-list
 "tlS" 'lsp-ui-sideline-mode
 "tlX" 'lsp-execute-code-action)

;; Increase amount of data read from process for lsp
(setq read-process-output-max (* 1024 1024))

;; Add lsp ui for higher level ui options
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  ;; Show lsp info on sideline
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; Extend lsp and treemacs integration
(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
    :after lsp-mode
    :config (dap-auto-configure-mode))
(use-package dap-mode
  :after lsp-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

;; Enable Flycheck for syntax checking.
;; Defer loading until used with lsp-mode
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Easier Commenting, not just for evil-mode
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Magit is one of THE killer features
(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

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
  "gr"  'magit-rebase)

;; Add Flycheck to elisp mode
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(pet/leader-keys
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(pet/leader-keys
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

;; Load Octave Mode automatically for specified files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.sci$" . octave-mode) auto-mode-alist))

;; Setup Octave Mode
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))

;; Use Infodocs within Emacs
(autoload 'octave-help "octave-hlp" nil t)

;; Integrated environment for TeX
  ;;(use-package auctex
  ;;  :config
  ;;  ;; enable completion
  ;;  (setq-default TeX-master nil)
  ;;  ;; 
  ;;  (setq TeX-parse-self t)
  ;;  ;; enable auto saving tex files
  ;;  (setq TeX-auto-save t))

  ;; LatexMK support for AUCTeX
  ;; (use-package auctex-latexmk)

  ;; Useful features for LaTeX-mode
  ;;(use-package latex-extra)

  ;; Fast input methods for LaTeX environments and math
  ;; (use-package cdlatex
  ;;   :bind (:map cdlatex-mode-map
  ;;               (nil . cdlatex-math-symbol)
  ;;               ("C-`" . cdlatex-math-symbol)
  ;;          :map org-cdlatex-mode-map
  ;;          (nil . cdlatex-math-symbol)
  ;;          ("C-`" . cdlatex-math-symbol))
  ;; )              

;;   (require 'tex)
;;   ; default compiled document: pdf
;;   (TeX-global-PDF-mode t)            
;;   (setq TeX-view-program-list
;; 	'(("zathura" "zathura --page=%(outpage) %o")))
;; 
;;   (setq TeX-view-program-selection
;; 	'(((output-dvi has-no-display-manager) "dvi2tty")
;; 	  ((output-dvi style-pstricks) "dvips and gv")
;; 	  (output-dvi "xdvi")
;; 	  (output-pdf "zathura")
;; 	  (output-html "xdg-open")))

;; Customize Python Mode for emacs, add lsp
(use-package python-mode
  :straight nil
  :hook (python-mode . lsp-deferred)
  :custom
  ; (python-shell-interpreter "python3")
  (dab-python-executable "python")
  (dab-python-debugger 'debugpy)
  :config
  (require 'dab-python)
  )

;; Setup lsp-pyright Server
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))  ; or lsp

;; Enable Virtual Environment Support
(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; Setup Automatic Tangling of Files

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-config ()
  (when (string-equal
         (buffer-file-name)
         (concat pet/dotfiles-dir
                 "000_OrgFiles/EmacsConfig.org"))
    ;; Have the user confirm tangle
    (let ((org-confirm-babel-evaluate t))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             #'pet/org-babel-tangle-config)))

;; Automatically tangle test config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-testconfig ()
  (when (string-equal
	 (buffer-file-name)
	 (concat pet/dotfiles-dir
		 "000_OrgFiles/EmacsTestConfig.org"))
    ;; Have user confirm tangle 
    (let ((org-confirm-babel-evaluate t))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook
	     'after-save-hook
	     #'pet/org-babel-tangle-testconfig)))

;; Automatically tangle backup config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-backupconfig ()
  (when (string-equal
	 (buffer-file-name)
	 (concat pet/dotfiles-dir
		 "000_OrgFiles/EmacsBackupConfig.org"))
    ;; Have user confirm tangle 
    (let ((org-confirm-babel-evaluate t))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook
	     'after-save-hook
	     #'pet/org-babel-tangle-backupconfig)))

;; Setup Automatic Tangling of Files

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-feeds ()
  (when (string-equal
         (buffer-file-name)
         (concat pet/dotfiles-dir
                 "000_OrgFiles/EmacsRSSFeed.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             #'pet/org-babel-tangle-feeds)))
