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


;; Save Home Dir for later use
(setq pet/home-dir
  (convert-standard-filename
   (expand-file-name "~/")))

;; Save Dotfiles Dirs for later use
(setq pet/dotfiles-dir
  (concat pet/home-dir
	  (convert-standard-filename
	   ".dotfiles/")))

(setq pet/dotfiles-emacsconfig-dir
	(concat pet/dotfiles-dir
		(convert-standard-filename
		 "C01_EmacsConfiguration/")))

;; Adjust font size to match your system
(defvar pet/default-font-size 140)
(defvar pet/default-variable-font-size 120)

;; Returns the color substring from given range
(defun pet/substring-from-range (str range)
  "Return substring from a given STR by specified RANGE"
  (substring str (first range) (second range)))

;; Return a range for specified colorchannel in a 6-digit
;; hexnumber
(defun pet/colorchannel-into-range (color)
  "Returns the range to look for a specified color.
  Inputs must be in 'rgbRGB' " 
  (setq color (s-lower-camel-case color))
  (cond ((equal color "r") '(1 3))
	((equal color "g") '(3 5))
	((equal color "b") '(5 7))))

;; Return the substring for a specified
(defun pet/colorsubstr-from-colorstr (colorstr colorchannel)
  "Returns the channelstr of the specified colorchannel from
   colorstrings like「#011f00」"
  (setq range (pet/colorchannel-into-range colorchannel))
  (pet/substring-from-range colorstring range))

;; Return the value of a specified colorchannel
(defun pet/number-from-string-by-channel
    (colorstring colorchannel)
  "Returns the numeric value of the specified colorchannel from
   colorstrings like「#011f00」"
  (string-to-number
  (pet/colorsubstr-from-colorstr
   (colorstr colorchannel)) 16))

;; Calculate Color average across channels from colorstr
(defun pet/avg-color (color)
  "Calculates the Color Average from COLOR"
  (/ (+ (pet/number-from-string-by-channel color "r")
	(pet/number-from-string-by-channel color "g")
	(pet/number-from-string-by-channel color "b"))
     3))

;; Returns a Color that contrasts background
(defun pet/contrast-color (bg-avg-decimal)
  "Returns the foreground color based on the avg background 
  being below 128. Returns White 「#000000」 if average is
  above"
  (if (> bg-avg-decimal 128) "#000000" "#ffffff"))

;; Takes a color string like #ffe0e0 and returns a light
;; or dark foreground color to make sure text is readable.
(defun pet/fg-from-bg (bg)
  "Returns the foreground color based on the avg background
   being below 128. Returns White 「#000000」 if average is
   above"
  (setq avg (pet/avg-color bg))
  (pet/contrast-color avg))

;; Function to Color Hexstring with their corresponding Colors
;; in RGB format
(defun pet/syntax-color-rgb ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in
  current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef0-9]\\{3\\}[^ABCDEFabcdef0-9]"
  (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list
		 :background (let* (
		  (ms (match-string-no-properties 0))
		  (r (pet/colorsubstr-from-colorstr bgstr "r"))
		  (g (pet/colorsubstr-from-colorstr bgstr "g"))
		  (b (pet/colorsubstr-from-colorstr bgstr "b"))
			     )
		  (concat "#" r r g g b b))))))
     ("#[ABCDEFabcdef0-9]\\{6\\}"
  (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background
		  (match-string-no-properties 0)))))))
  (font-lock-flush))

;; Function to Colorstring with their corresponding Colors
;; in HSL format
(defun pet/syntax-color-hsl ()
  "Syntax color CSS's HSL color spec eg 「hsl(0,90%,41%)」 in
  current buffer."
  (interactive)
  (require 'color)
  (font-lock-add-keywords
   nil
   '(("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *,
    *\\([0-9]\\{1,3\\}\\)% *)"
  (0 (put-text-property
	  (+ (match-beginning 0) 3)
	  (match-end 0)
	  'face
	  (list
	   :background
	   (concat
	    "#"
	    (mapconcat
	     'identity
	     (mapcar
	  (lambda (x) (format "%02x" (round (* x 255))))
	  (color-hsl-to-rgb
	   (/ (string-to-number (match-string-no-properties 1)) 360.0)
	   (/ (string-to-number (match-string-no-properties 2)) 100.0)
	   (/ (string-to-number (match-string-no-properties 3)) 100.0)))
	     "" )) ;  "#00aa00"
	   ))))))
  (font-lock-flush))

;; Function to insert a random color in HSL format
(defun pet/insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
  Sample output: hsl(100,24%,82%);"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);"
		  (random 360) (random 100) (random 100))))

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
(set-face-attribute 'default nil :font "Iosevka"
		    :height pet/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka"
		    :height pet/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell"
		    :height pet/default-font-size
		    :weight 'regular)

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

;; Set tabs to be 4 spaces
(setq-default indent-tabs-mode nil)
;; Set the default, fallback tabstop to be 4 spaces
(setq-default tab-stop-list (number-sequence 4 120 4))
;; Set Number of Spaces displayed for a tab stop
(setq-default tab-width 4)

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

;; Remember recently accessed files
(recentf-mode t)

;; Limit history file to 50 entries to speed up start
(setq history-length 50)
;; Save command and file history
(savehist-mode t)

;; Remember Cursor Positions on accessed files 
(save-place-mode t)

;; Avoid Clutter by saving Customization Settings to a different file
(setq custom-file (locate-user-emacs-file "customization_variables.el"))
(load custom-file 'no-error 'no-message)

;; Don't show windowed Dialog Box on Prompts
(setq use-dialog-box nil)

;; Revert Buffers when Files changed on disk
(global-auto-revert-mode t)

;; Automatically revert Dired (and similar) Buffers without confirmation
(setq global-auto-revert-non-file-buffers t)

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

(use-package visual-regexp)

;; Extend Emacs Emoji capability (apart from Unicode)
(use-package emojify
  ;; if you want to enable emojis globally:
  ;; :hook (after-init . global-emojify-mode)
  )

;; Setup general for easier key config
(use-package general
  :config
  (general-create-definer pet/leader-keys
  :prefix "C-."
  :global-prefix "C-.")

  (pet/leader-keys

    ;; Layouts
   "l"    '(:ignore t :which-key "Layout")

   ;; Editing Tools
   "e"     '(:ignore t :which-key "Editing Tools")
   ;; Letters
   "el"    '(:ignore t :which-key "Letters")
   "elM-u" 'upcase-initials
   "elC-uM-u" 'upcase-initials-region
   ;; Tabs
   "et"    '(untabify
         :which-key "Untabify")
   "er"    '(regexp-builder
         :which-key "Regexp Builder")

   ;; Files
   "f"   '(:ignore t :which-key "Files")
   "fR"   'recentf-open-files

   ;; Org Mode
   "o"    '(:ignore t :which-key "Org Mode")

   ;; Toggles
   "t"    '(:ignore t :which-key "Toggles")
   "tc"   'world-clock
   "tt"   '(counsel-load-theme
        :which-key "Choose Theme")

   ;; Toggles - Highlighting
   "th"   '(:ignore t :which-key "Highlighting")
   ;; Toggles - Highlighting - Colors
   "thc"  '(:ignore t :which-key "Colors")
   "thcr" '(pet/syntax-color-rgb
        :which-key "RGB")
   "thch" '(pet/syntax-color-hsv
        :which-key "HSV")
   ;; Toggles - Modes
   "tm"   '(:ignore t :which-key "Modes")
   "tmv"  '(visual-line-mode :which-key "Visual Line Mode")
   "tmw"  '(whitespace-mode :which-key "Whitspace Mode")
   "tme"  '(emojify-mode :which-key "Emojify Mode")
  ))

;; Add Dashboard to Emacs
(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
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

;; Enable Command Log Mode
(use-package command-log-mode)

;; Load Doom Themes
(use-package doom-themes
  :init (load-theme 'doom-dracula t)
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

    "ff"  '(counsel-find-file :which-key "open file")
    "C-f" 'counsel-find-file
    "fr"  '(counsel-recentf :which-key "recent files")
    "fR"  '(revert-buffer :which-key "revert file")
    "fj"  '(counsel-file-jump :which-key "jump to file"))
  )  
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
  (counsel-describe-function-function
   #'helpful-callable)
  (counsel-describe-variable-function
   #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
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
  ;; Set default file for states
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
		  (next-line) (call-interactively
			   'elfeed-search-show-entry)))
	 ("p" . (lambda () (interactive)
		  (previous-line) (call-interactively
				   'elfeed-search-show-entry)))
	 ("m" . (lambda () (interactive)
		  (apply 'elfeed-search-toggle-all '(star))))
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
  '("%F %R" 16 :left)))
;; Load Feeds and Feed Settings  
(load (concat pet/dotfiles-emacsconfig-dir
	  "EmacsRSSFeed.el"))

;; Snippet for periodic update for feeds
;; (add-to-list 'elfeed-update-hooks 'elfeed-update)
;; (run-with-timer 0 (* 60 60 4) 'elfeed-update)

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

;; Add Filters by file extension to dired buffer
(use-package dired-filter)

;; Add Ranger Directory Explorer
(use-package ranger
  :config
  ;; I don't want ranger to be the default
  (setq ranger-override-dired-mode nil)
  )

;; Helper Functions for Org
(defun pet/org-font-setup ()
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

;; Replace list hyphen with dot
(defun pet/org-replace-hyphen ()
  (font-lock-add-keywords
   'org-mode '(("^ *\\([-]\\) "
		(0 (prog1 () (compose-region
			  (match-beginning 1)
			  (match-end 1) "•"))))))
  )

;; Helper Function to quickly toggle Babel Confirm Evaluation
(defun pet/org-toggle-babel-confirm-evaluate ()
(interactive)
"Toogle org-babel-confirm-evaluate on/ff"
(if org-confirm-babel-evaluate
    (setq org-confirm-babel-evaluate nil)
  (setq org-confirm-babel-evaluate t))
(print (concat "Org Babel Confirm State: "
	   (format "%s" org-confirm-babel-evaluate))))

;; Store Org Directory
(setq pet/org-dir
  (concat pet/home-dir
	  (convert-standard-filename
	   "Org/")))

;; Setting Up Org Mode
(use-package org
  :bind (("C-c l" . org-store-link))
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory pet/org-dir)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Setup inline previewing of latex fragments
  (setq org-latex-create-formula-image-program
	'imagemagick)

  ;; Specify Agenda Files
  (setq org-agenda-files
	(cons (concat pet/org-dir "journal")
	  ;; Add Files a starting with "personal-"
	  (directory-files pet/org-dir t
			   "personal-\\(tasks\\|mail\\|chores\\|contracts\\)-?[A-Za-z]*.org")
	  ))

  ;; Set Org Clock Sound File
  (setq org-clock-sound (concat pet/org-dir "sounds/Rush.wav"))


  ;; Startup with inline images displayed
  (setq org-startup-with-inline-images t)


  ;; Enable helper function replacing hyphen
  (pet/org-replace-hyphen)


  ;; Customize Apps for Filelinks
  (cl-loop for type in
	   ;; Open PDFs with Zathura
	 '(("\\.pdf\\'" . "zathura %s")
	   ;; Open Pictures with sxiv 
	   ("\\.png\\'" . "sxiv %s")
	   ("\\.jpg\\'" . "sxiv %s")
	   ("\\.jpeg\\'" . "sxiv %s")
	   ;; Open Youtube links with freetube
	   ("\\.\\*youtu\\.\\*" . "freetube %s")
	   )
	 do
	 (add-to-list 'org-file-apps type))

  ;; Add Custom TODO Keywords - in 2 seperate Sequences
  (setq org-todo-keywords
	;; Sequence 1 
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  ;; Sequence 2
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)"
		    "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)"
		    "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Set Keywords with shortcuts
  (setq org-tag-alist
	'((:startgroup)
	  ;; Put mutually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("@study" . ?S)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))

  ;; Set Refile Targets to be considered, Emphasis on Archive 
  (setq org-refile-targets
    '(("personal-archive.org" :maxlevel . 1)
  ("personal-tasks.org" :maxlevel . 1)))

  ;; The default here is 999, which is a little to constricting for SQL and such
  (setq org-table-convert-region-max-lines 9999)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (pet/leader-keys
    "ot" '(:ignore t :which-key "Toggle")
    "otb" '(pet/org-toggle-babel-confirm-evaluate
	    :which-key "Babel Confirm Evaluation")
    "oti" '(org-toggle-inline-images
	    :which-key "Inline Images")
    "otp" '(org-toggle-pretty-entities
	    :which-key "Pretty entities")
    "oi" '(:ignore t :which-key "Import")
    "oit" '(org-table-import
	    :which-key "Table")
    )
  )

;; Setup Org Superstar
(use-package org-superstar
  :after org)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

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

;; (use-package ob-ipython)

;; (require-package 'ob-ipython)

;; enable/disable languages for org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)    ;; Elisp
    (lisp . t)          ;; Lisp
    (clojure . t)       ;; Clojure     
    (scheme . t)        ;; Scheme
    (python . t)        ;; Python
    ;; (ipython . t)       ;; IPython

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
    (sed . t)           ;; GNUsed
    (css . t)           ;; CSS
    ))         

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
           ;; ("si" . "src ipython :session :async :exports both :results raw drawer")
           ;; This is an alternative Block
           ;; For IPython
           ;; ("si" . "src ipython :session :async :results output")
           )
         do
         (add-to-list
          'org-structure-template-alist block))

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
	 `(("a" "Acronyms" table-line
	(file+headline "~/Org/acronyms.org" "Inbox")
	"| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

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

	   ("ls" "Shopping List" checkitem
	(file+olp "~/Org/lists-shopping.org" "Inbox")
	"[ ] %^{Itemname}")

	   ("ll" "Literature" checkitem
	(file+olp "~/Org/lists-literature.org" "Inbox")
	"[ ] %^{Author} - %^{Titel}")

	   ("lm" "Music" checkitem
	(file+olp "~/Org/lists-music.org" "Inbox")
	"[ ] %^{Interpret} - %^{Title}")

	   ("q" "Quotes" entry
	(file+olp "~/Org/quotes.org" "Inbox")
	"* %^{Originator}\n\n#+begin_quote\n%?\n#+end_quote")

	   ("t" "Tasks / Projects")
	   ("tt" "TODO Task" entry (file+olp
				"~/Org/personal-tasks.org" "Inbox")
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

  ;; Activate Context Templates for Email 
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
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
	   :unnarrowed t)
	  ("w" "wiki")
	  ("wn" "wiki node" plain
	   "\n* ${title}\n\n%?" 
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			  "\n#+filetags: :%^{filetag}:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
	   :unnarrowed t)
	  ("wi" "wiki index node" plain
	   "\n* ${title} Kompendium Index\n\n%?" 
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
				  "\n#+filetags: :index:%^{filetag}:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
	   :unnarrowed t)
	  ("wr" "wiki references node" plain
	   "\n* References\n%?\n** Websites\n\n** Literature" 
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
				  "\n#+filetags: :%^{filetag}:references:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
	   :unnarrowed t)
	  ("l" "programming language" plain
	   "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
	   :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
	   :unnarrowed t)  
	  ("b" "book notes" plain (file "~/.dotfiles/00_OrgFiles/Templates/RoamCapture-BookNoteTemp.org")
	   :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
	   :unnarrowed t)
	  ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	   :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+filetags: Project")
	   :unnarrowed t)
	  ))


   ;; dailies capture template
  (setq org-roam-dailies-capture-templates
	`(("d" "default" entry "* %<%I:%M %p>: %?"
	   :if-new (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))

  (org-roam-setup)
  ;; Ensure the keymap is available
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  (pet/leader-keys
"or"  '(:ignore t :which-key "Org Roam")
"ort" 'org-roam-tag-add
"ora" 'org-roam-alias-add
"ord" 'org-roam-diagnostics
"oru" 'org-roam-ui-open
)
  )

;; Helper Function to insert org note immediately
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
	(org-roam-capture-templates
	 (list (append (car org-roam-capture-templates)
		   '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; A Visualization of your org roam node structure
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
  :hook ((c-mode          ;; clangd
          c++-mode        ;; clangd
          c-or-c++-mode   ;; clangd
          python-mode     ;; pyright
          typescript-mode ;; ts-ls (tsserver wrapper)
          js-mode         ;; ts-ls (tsserver wrapper)
          web-mode        ;; ts-ls/HTML/CSS
          ) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
      ("SPC TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil)
  :config (lsp-enable-which-key-integration t)
  ;; automatically set project root as determined by projectile
  ;; (setq lsp-auto-guess-root t)
  ;; Disable logging of all language server message for performance
  (setq lsp-log-io nil)
  ;; Set LSP Restart to auto (interactive by default)
  ;; (setq lsp-restart 'auto-restart)
  ;; disable symbol references
  (setq lsp-enable-symbol-highlighting nil)
  ;; disable on type formatting
  (setq lsp-enable-on-type-formatting nil)
  ;; disable signature conditions and documentation
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  ;; disable eldoc hook
  (setq lsp-eldoc-hook nil)
  ;; disable modeline informations
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  ;; disable breadcrumb/headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; disable semantic tokens
  (setq lsp-semantic-tokens-enable nil)
  ;; disable code folding
  (setq lsp-enable-folding nil)
  ;; dont enable imenu automatically
  (setq lsp-enable-imenu nil)
  ;; disable snippet completion
  (setq lsp-enable-snippet nil)
  ;; Set delay (0.5 is default)
  (setq lsp-idle-delay 0.5)
  ;; Increase amount of data read from process for lsp (1MB)
  (setq read-process-output-max (* 1024 1024))

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
  )

;; Add lsp ui for higher level ui options
(use-package lsp-ui
  :commands lsp-ui-mode
  ;; :hook (lsp-mode . lsp-ui-mode)
  ;; Show lsp info on sideline
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-doc-position 'bottom)
  )

;; Extend lsp and treemacs integration
(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
;;(use-package dap-mode
;;  :after lsp-mode
;;  :custom
;;  (lsp-enable-dap-auto-configure nil)
;;  :config
;;  (dap-ui-mode 1)
;;  (dap-tooltip-mode 1)
;;  (require 'dap-node)
;;  (dap-node-setup))

;; Enable Flycheck for syntax checking.
;; Defer loading until used with lsp-mode
(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Easier Commenting, not just for evil-mode
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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
  "E"   '(:ignore t :which-key "eval")
  "Eb"  '(eval-buffer :which-key "eval buffer"))

(pet/leader-keys
  :keymaps '(visual)
  "Er" '(eval-region :which-key "eval region"))

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
(use-package tex-site
  :straight auctex)

;; enable completion
(setq-default TeX-master nil)
(setq TeX-parse-self t)
;; enable auto saving tex files
(setq TeX-auto-save t)

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
  ;; don't hook lsp straight away
  ;; :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python")
  (dab-python-executable "python")
  (dab-python-debugger 'debugpy)
  :config
  (require 'dab-python)
  )

;; Setup lsp-pyright Server
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  ;; Use Python 3 in case Python 2 is installed as well
  :init (when (executable-find "python3")
        (setq lsp-pyright-python-executable-cmd "python3"))
  )


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

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-qtile ()
  (when (string-equal
	 (buffer-file-name)
	 (concat pet/dotfiles-dir
		 "000_OrgFiles/QtileConfig.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook
	     'after-save-hook
	     #'pet/org-babel-tangle-qtile)))

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-qtilebackup ()
  (when (string-equal
	 (buffer-file-name)
	 (concat pet/dotfiles-dir
		 "000_OrgFiles/QtileBackupConfig.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook
	     'after-save-hook
	     #'pet/org-babel-tangle-qtilebackup)))

;; Setup Automatic Tangling of Run Launchers

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-rofi ()
  (when (string-equal
	 (buffer-file-name)
	 (concat pet/dotfiles-dir
		 "000_OrgFiles/RofiConfig.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook
	     'after-save-hook
	     #'pet/org-babel-tangle-rofi)))
