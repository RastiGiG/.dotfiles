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

;; Save Dotfiles Dir for later use
(setq pet/dotfiles-dir
  (concat pet/home-dir
      (convert-standard-filename
       ".dotfiles/")))

;; Save Template Dir for later use
(setq pet/temp-dir
  (concat pet/home-dir
      (convert-standard-filename
       "Templates/")))

;; Save Emacs Template Dir for later use
(setq pet/latex-header-temp-dir
  (concat pet/temp-dir
      (convert-standard-filename
       "X2_LaTeX_Templates/00-Headers/")))

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

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 'relative))))

;; Set Visual Line Mode for text modes only
;; Preferred over global-visual-line-mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Enable Highlight-Line
(hl-line-mode 1)

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

;; Use specific Fontsets for Symbols
(setq use-default-font-for-symbols nil)

;; Use Symbols Nerd Font as Default Symbols Font, otherwise fall back to Symbola (or else)
(set-fontset-font t 'unicode "Symbols Nerd Font")
(set-fontset-font t '(#xF500 . #xF8FF) "Symbols Nerd Font")
(set-fontset-font t 'unicode "Symbola" nil 'append)
(set-fontset-font t 'unicode (font-spec :script 'unicode) nil 'append)

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
(setq-default indent-tabs-mode t)
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

;; Package to setup Path Variable (and more) in Emacs
(use-package exec-path-from-shell)

;; Read Path from Shell Setup when Emacs Server is launched through SystemD
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Copy values of other Environment Variables
;; (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;   (add-to-list 'exec-path-from-shell-variables var))

;; Article Skeleton
(define-skeleton pet/latex-article-skeleton
  "Skeleton for article type latex documents"
  "Preamble:"
  "\\documentclass{article}\n"
  "\\usepackage[utf8]{inputenc}\n"
  "\\usepackage[margin=1 in]{geometry}\n"
  "\\usepackage{graphicx}\n"
  "\\setlength{\\parindent}{4em}\n"
  "\\setlength{\\parskip}{1em}\n"
  "\\renewcommand{\\baselinestretch}{1.5}\n\n"
  "\\author{<AUTOR>}\n"
  "\\title{"_"}\n"
  "\\date{\\today}\n\n"
  "\\begin{document}\n"
  "\\maketitle\n\n"
  "\\end{document}\n")

;; Org LaTeX Summary Header 
(define-skeleton pet/org-latex-summary-skeleton
  "Skeleton for summaries "
  "Preamble:"
  "#+LATEX_CLASS: article\n"
  "#+LATEX_CLASS_OPTIONS: [a5paper,landscape,fourcolumn]\n"
  "#+LATEX_COMPILER: lualatex\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "summaryheader.tex}\n"))
  "#+STARTUP: showeverything\n"
  "#+OPTIONS: toc:nil\n"
  "\\begin{multicols*}{4}\n"
  "* "_"\n"
  "\\end{multicols*}\n")

;; Org LaTeX Article Header
(define-skeleton pet/org-latex-article-skeleton
  "Skeleton for articles "
  "Preamble:"
  "#+STARTUP: showeverything\n"
  "#+TITLE: TITLE\n"
  "#+AUTHOR: AUTHOR\n"
  "#+DATE: \\today\n"
  "#+LATEX_CLASS: article\n"
  "#+LATEX_CLASS_OPTIONS: [a4paper]\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "articleheader.tex}\n"))
  "#+OPTIONS: toc:nil\n")

;; Org LaTeX Beamer Header
(define-skeleton pet/org-latex-beamer-skeleton
  "Skeleton for articles "
  "Preamble:"
  "#+STARTUP: beamer\n"
  "#+TITLE: TITLE\n"
  "#+AUTHOR: AUTHOR\n"
  "#+DATE: \\today\n"
  "#+LaTeX_CLASS: beamer\n"
  "#+LaTeX_CLASS_OPTIONS: [final]\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "beamerheader.tex}\n"))
  "#+STARTUP: showeverything\n"
  "#+OPTIONS: toc:nil\n")

;; Org Wiki
(define-skeleton pet/org-wiki-entry-skeleton
  "Skeleton for articles "
  "Preamble:"
  "#+STARTUP: showeverything\n"
  "#+TITLE: "_"\n"
  "#+AUTHOR: AUTHOR\n"
  "#+STARTUP: showeverything\n"
  "\n"
  "* Index")

;; Org Wiki Index
(define-skeleton pet/org-wiki-index-skeleton
  "Skeleton for articles "
  "Preamble:"
  "#+STARTUP: showeverything\n"
  "#+TITLE: "_"\n"
  "#+AUTHOR: AUTHOR\n"
  "#+STARTUP: showeverything\n"
  "\n"
  "* Index\n"
  "\n"
  "** Summaries\n"
  "[[./summaries/summaries.org][Summaries]]"
)

;; Bind Ace Window Control
(global-set-key (kbd "M-o") 'ace-window)

;; Yasnippets
(use-package yasnippet
  :init
  ;; save Yasnippet dir
  (setq pet/yasnippet-dir
        (concat pet/dotfiles-emacsconfig-dir
                "snippets"))

  :config
  ;; Set Yasnippet dir
  (setq yas-snippet-dirs '(pet/yasnippet-dir))

  ;; Activate Yasnippets globally
  (yas-global-mode 1)

  ;; Allow Stacked Expansion (Expansion within Expansion)
  ;; (setq yas-triggers-in-field t)

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

;; Setup general for easier key config
(use-package general
      :config
      (general-create-definer pet/leader-keys
      :prefix "C-."
      :global-prefix "C-.")

      (pet/leader-keys

	;; Layouts
	"l"     '(:ignore t :which-key "Layout")


	;; Authentication
	"a"     '(:ignore t :which-key "Authentification")


	;; Bookmarks
	"b"     '(:ignore t :which-key "Bookmarks")
	"bs"    '(bookmark-set :which-key "Set Bookmark")
	"bl"    '(bookmark-bmenu-list :which-key "bookmark list")


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
	"f"     '(:ignore t :which-key "Files")
	"fR"    'recentf-open-files


	;; Org Mode related
	"o"     '(:ignore t :which-key "Org Mode")


	;; Toggles
	"t"     '(:ignore t :which-key "Toggles")
	"tc"    'world-clock
	"tt"    '(counsel-load-theme
			      :which-key "Choose Theme")
	;; Toggles - Highlighting
	"th"    '(:ignore t :which-key "Highlighting")
	;; Toggles - Highlighting - Colors
	"thc"   '(:ignore t :which-key "Colors")
	"thcr"  '(pet/syntax-color-rgb
			      :which-key "RGB")
	"thch"  '(pet/syntax-color-hsv
			      :which-key "HSV")
	;; Toggles - Modes
	"tm"    '(:ignore t :which-key "Modes")
	"tmv"   '(visual-line-mode :which-key "Visual Line Mode")
	"tmh"   '(hl-line-mode :which-key "Highlight Line Mode")
	"tmw"   '(whitespace-mode :which-key "Whitspace Mode")
	"tmo"   '(org-mode :which-key "Org Mode")
	"tmf"   '(origami-mode :which-key "Origami Mode")
	"tmf"   '(follow-mode :which-key "Follow Mode")
	"tme"   '(emojify-mode :which-key "Emojify Mode")
	"tms"   '(scroll-all-mode :which-key "Scroll All Mode")
	))

;; applies beacon effect to the highlighted line on page scrolls
(use-package beacon
   :config
   (beacon-mode 1)
   ;(setq beacon-color 0.4)
   )

;; Display battery for when in full screen mode
(display-battery-mode t)

;; Don't show windowed Dialog Box on Prompts
(setq use-dialog-box nil)

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

;; Enable Winner Mode
(winner-mode 1)

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

;; Visually Mark Regexp
(use-package visual-regexp)

;; Extend Emacs Emoji capability (apart from Unicode)
(use-package emojify
  ;; if you want to enable emojis globally:
  ;; :hook (after-init . global-emojify-mode)
  )

;; Add Origami Mode for Folding
(use-package origami
  :hook (yaml-mode . origami-mode)
  :bind (
         :map origami-mode-map
              ("<tab>" . origami-recursively-toggle-node)
              ("S-<tab>" . origami-toggle-all-nodes)
              ("C-c C-n" . origami-next-fold)
              ("C-c C-p" . origami-previous-fold)
              ("C-c C-S-_" . origami-undo)
              ("C-c C-S-M-_" . origami-redo))
  )

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

;; Load Hydra Package
(use-package hydra
      :config

      ;; Add leader key Menu
      (pet/leader-keys
	"h" '(:ignore t :which-key "Hydras")
	)
      )

;; Define Text Scale Hydra 
(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(pet/leader-keys
  "hs" '(hydra-text-scale/body :which-key "Scale text")
 )

;; Hydra for Buffer Menu functions
(defhydra hydra-buffer-menu (
                             :hint nil
                             :timeout 10
                             ;; :color pink
                             )
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

;; Access Hydra in Buffer Menu with '.'
(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;; Bookmark Menu
(defhydra hydra-bookmark-menu (
							       :color pink
									      :hint nil
									      :timeout 10)
      "

	      ^^^Mark^             ^Actions^            ^Search^            ^Annotations^         ^Open Bookmark
	      ^^^^^^^^-----------------------------------------------------------------------------------------------------
	      _m_: mark         _x_: execute          _/_: isearch             _a_: show         _o_   on other window 
	      _u_: unmark       _r_: rename           _l_: locate              _A_: show all     _C-o_ switch other window    
	      _U_: unmark up    _R_: relocate bmk     _S_: show filenames      _e_: edit         _1_   on full window
	      _d_: delete       _w_: write bmk list   _T_: hide filenames      ^ ^               _2_   on split vertical
	      _D_: delete up    _i_: import bmk list  _t_: toggle filenames    ^ ^               _5_   on other frame
	      "
      ("m" bookmark-bmenu-mark)
      ("u" bookmark-bmenu-unmark)
      ("U" bookmark-bmenu-backup-unmark)
      ("d" bookmark-bmenu-delete)
      ("D" bookmark-bmenu-delete-backwards)
      ("x" bookmark-bmenu-execute-deletions)
      ("r" bookmark-bmenu-rename)
      ("R" bookmark-bmenu-relocate)  
      ("w" bookmark-bmenu-save)                   ;; 'write' bookmark list
      ("i" bookmark-bmenu-load)                   ;; 'import' bookmark list
      ("/" bookmark-bmenu-search)
      ("l" bookmark-bmenu-locate)
      ("S" bookmark-bmenu-show-filenames)  
      ("T" bookmark-bmenu-hide-filenames)
      ("t" bookmark-bmenu-toggle-filenames)
      ("a" bookmark-bmenu-show-annotation)
      ("A" bookmark-bmenu-show-all-annotations)
      ("e" bookmark-bmenu-edit-annotation)
      ("c" nil "cancel" :exit t)
      ("s" bookmark-bmenu-select "select" :color blue)
      ("o" bookmark-bmenu-other-window :color blue)
      ("C-o" bookmark-bmenu-switch-window :color blue)
      ("1" bookmark-bmenu-1-window :color blue)
      ("2" bookmark-bmenu-2-window :color blue)
      ("5" bookmark-bmenu-other-frame :color blue)
      ("q" quit-window "quit bm list" :color blue))

;; Access Menu through '.' in Bookmark List
(with-eval-after-load "bookmark"
      (define-key bookmark-bmenu-mode-map
			      "." 'hydra-bookmark-menu/body))

;; Apropos Hydra
(defhydra hydra-apropos (
                         ;; :color blue
                         :hint nil
                               )
  "
^Apropos
^^^^^^^^-----------------------
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_
"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))
;; Recommended binding:
;; (global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; Add to Leader keys
(pet/leader-keys
  "ha" '(hydra-apropos/body :which-key "Apropos")
 )

;; Window Management Helpers
(require 'windmove)

;; Move Splitter left
(defun pet/move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'right))
	      (shrink-window-horizontally arg)
	(enlarge-window-horizontally arg)))

;; Move Splitter left
(defun pet/move-splitter-right (arg)
	"Move window splitter right."
	(interactive "p")
	(if (let ((windmove-wrap-around))
		      (windmove-find-other-window 'right))
		(enlarge-window-horizontally arg)
	      (shrink-window-horizontally arg)))

 ;; Move Splitter Up
(defun pet/move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'up))
	      (enlarge-window arg)
	(shrink-window arg)))

;; Move Splitter Down
(defun pet/move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'up))
	      (shrink-window arg)
	(enlarge-window arg)))

;; Define Window Management Hydra
(defhydra hydra-window (
						:hint nil
							      )
      "
	Movement^^        ^Split^         ^Switch^		^Resize^
	----------------------------------------------------------------
	_M-<left>_  ←	_v_ertical    	_b_uffer		_<left>_  X←
	_M-<down>_  ↓   	_x_ horizontal	_f_ind files	_<down>_  X↓
	_M-<up>_    ↑   	_z_ undo      	_a_ce 1	    	_<up>_    X↑
	_M-<right>_ →   	_Z_ reset      	_s_wap	     	_<right>_ X→
	_F_ollow Mode    	_D_lt Other   	_S_ave	     max_i_mize
	_SPC_ cancel	    _o_nly this   	_d_elete	
	"
      ;; Movement
      ("M-<left>"  windmove-left)
      ("M-<down>"  windmove-down)
      ("M-<up>"    windmove-up)
      ("M-<right>" windmove-right)

      ;; Resize
      ("<left>"  pet/move-splitter-left)
      ("<down>"  pet/move-splitter-down)
      ("<right>" pet/move-splitter-right)
      ("<up>"    pet/move-splitter-up)

      ("b" list-buffers)
      ("f" find-files)
      ("F" follow-mode)
      ("a" (lambda ()
		 (interactive)
		 (ace-window 1)
			 (add-hook 'ace-window-end-once-hook
					       'hydra-window/body))
       )
      ("v" (lambda ()
		 (interactive)
		 (split-window-right)
		 (windmove-right))
       )
      ("x" (lambda ()
		 (interactive)
		 (split-window-below)
		 (windmove-down))
       )
      ("s" (lambda ()
		 (interactive)
		 (ace-window 4)
		 (add-hook 'ace-window-end-once-hook
				       'hydra-window/body)))
      ("S" save-buffer)
      ("d" delete-window)
      ("D" (lambda ()
		 (interactive)
		 (ace-window 16)
		 (add-hook 'ace-window-end-once-hook
				       'hydra-window/body))
       )
      ("o" delete-other-windows)
      ("i" ace-maximize-window)
      ("z" (progn
		 (winner-undo)
		 (setq this-command 'winner-undo))
       )
      ("Z" winner-redo)
      ("SPC" nil)
      )

;; Add to Leader keys
(pet/leader-keys
      "hw" '(hydra-window/body :which-key "Window Management")
      )

;; hydra multiple cursors
(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^             ^Other^
--------------------------------------------------------
[_p_]   Previous    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip        [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark      [_M-n_] Unmark  [_r_] Mark by regexp
^ ^                 ^ ^             [_d_] Mark all defun
^ ^                 ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("d" mc/mark-all-like-this-in-defun :exit t)
  ("q" nil))

;; Add to Leader keys
(pet/leader-keys
  "hm" '(hydra-multiple-cursors/body :which-key "Multiple Cursors")
 )

;; Editing Toggles
(defhydra hydra-editing-visuals (
						:color pink
							       :hint nil
							       )
      "
^Editing Visuals
^^^^^^-------------------------------------------------------------------------
_a_ abbrev-mode:                         %`abbrev-mode
_C_ display-fill-column-indicator-mode:  %`display-fill-column-indicator-mode
_d_ debug-on-error:                      %`debug-on-error
_f_ auto-fill-mode:                      %`auto-fill-function
_F_ variable-pitch-mode                 
_i_ toggle-input-method                 
_n_ display-line-numbers-mode:           %`display-line-numbers-mode
_M_ doom-modeline-mode:                  %`doom-modeline-mode
_R_ read-only-mode                      
_t_ truncate-lines:                      %`truncate-lines
_T_ counsel-load-theme                  
_v_ visual-line-mode:                    %`visual-line-mode
_w_ whitespace-mode:                     %`whitespace-mode
"
      ("a" abbrev-mode)
      ("C" display-fill-column-indicator-mode)
      ("d" toggle-debug-on-error)
      ("f" auto-fill-mode)
      ("F" variable-pitch-mode)
      ("i" toggle-input-method)
      ("t" toggle-truncate-lines)
      ("T" counsel-load-theme)
      ("v" visual-line-mode)
      ("n" display-line-numbers-mode)
      ("M" doom-modeline-mode)
      ("w" whitespace-mode)
      ("R" read-only-mode)
      ("q" nil "quit" :exit 1))

;; (global-set-key (kbd "C-c C-v") 'hydra-editing-toggles/body)

;; Add to Key Space
(pet/leader-keys
      "eh" '(hydra-editing-visuals/body :which-key "Editing Visuals")
      "T"  '(hydra-editing-visuals/body :which-key "Toggle Hydra")
      "ht" '(hydra-editing-visuals/body :which-key "Editing Visuals")
      )

;; Mu4e Hydra
(defhydra hydra-mu4e-headers (
							      :color blue
									 :hint nil
									 )
      "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups 
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------ 
_C_: compose | _}_: next query    | _a_: action  | _|_: to shell  | _´_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_h_: ?mode   | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

      ;; general
      ("n" mu4e-headers-next)
      ("p" mu4e-headers-previous)
      ("[" mu4e-select-next-unread)
      ("]" mu4e-select-previous-unread)
      ("y" mu4e-select-other-view)
      ("R" mu4e-compose-reply)
      ("C" mu4e-compose-new)
      ("F" mu4e-compose-forward)

      ;; search
      ("s" mu4e-headers-search)
      ("S" mu4e-headers-search-edit)
      ("/" mu4e-headers-search-narrow)
      ("b" mu4e-headers-search-bookmark)
      ("B" mu4e-headers-search-bookmark-edit)
      ("{" mu4e-headers-search-prev :color pink)      ; differs from built-in - make sure to add them later
      ("}" mu4e-headers-search-next :color pink)      ; differs from built-in - make sure to add them later
      ("C-+" mu4e-headers-split-view-grow)
      ("C--" mu4e-headers-split-view-shrink)

      ;; mark stuff 
      ("!" mu4e-headers-mark-for-read)
      ("?" mu4e-headers-mark-for-unread)
      ("r" mu4e-headers-mark-for-refile)
      ("u" mu4e-headers-mark-for-unmark)
      ("U" mu4e-mark-unmark-all)
      ("d" mu4e-headers-mark-for-trash)
      ("D" mu4e-headers-mark-for-delete)
      ("m" mu4e-headers-mark-for-move)
      ("a" mu4e-headers-action)                  ; not really a mark per-se
      ("A" mu4e-headers-mark-for-action)
      ("*" mu4e-headers-mark-for-something)


      ("#" mu4e-mark-resolve-deferred-marks)
      ("%" mu4e-headers-mark-pattern)
      ("&" mu4e-headers-mark-custom)
      ("+" mu4e-headers-mark-for-flag)
      ("-" mu4e-headers-mark-for-unflag)
      ("t" mu4e-headers-mark-subthread)
      ("T" mu4e-headers-mark-thread)

      ;; miscellany
      ("q" mu4e~headers-quit-buffer)
      ("H" mu4e-display-manual)
      ("h" describe-mode)
      ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

      ;; switches
      ("O" mu4e-headers-change-sorting)
      ("P" mu4e-headers-toggle-threading)
      ("Q" mu4e-headers-toggle-full-search)
      ("V" mu4e-headers-toggle-skip-duplicates)
      ("W" mu4e-headers-toggle-include-related)

      ;; more miscellany
      ("´" mu4e-update-mail-and-index)           ; differs from built-in
      (";" mu4e-context-switch)  
      ("j" mu4e~headers-jump-to-maildir)

      ("." nil))

;; Store Backups in a single directory
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
  ;; Enable Image preview
  (setq ranger-show-literal nil)
  ;; Set Max Preview Size to 50MB
  ;; !!careful, this can really slow down your machine!!
  (setq ranger-max-preview-size 50)
  ;; Don't preview video/audio files
  (setq ranger-excluded-extensions ' ("mkv" "iso" "mp4" "mp3"))
  (pet/leader-keys
    "tmr"  '(ranger-mode :which-key "Ranger Mode")
    )
  )

;; Add wrapper for the command line tool 'pass'
(use-package password-store
      :config
      ;; If you want to adjust the default password length
      ;; (setq password-store-password-length 12)

      ;; Use Password Store as Source for Auth-Sources
      (setq auth-sources '(password-store))
      )

;; Add Functions to Leader Keys
(pet/leader-keys
      "ap"  '(:ignore t :which-key "Password Store")
      "app" 'password-store-copy
      "api" 'password-store-insert
      "apg" 'password-store-generate)

;; Use EBDB for contact management
(use-package ebdb
      :config
      ;; Set the source files for Contact DBs
      (setq ebdb-sources (list                        
					      (concat pet/home-dir "Contacts/default-contacts.db")
					      (concat pet/home-dir "Contacts/family.db")
					      (concat pet/home-dir "Contacts/work.db")
					      (concat pet/home-dir "Contacts/organizations.db")
					      (concat pet/home-dir "Contacts/mailing-lists.db")
					      ))

      ;; Access Menu through '.' in EBDB Buffer
      ;; (define-key ebdb-mode-map
      ;;		  "." 'hydra-ebdb-menu/body)		  

      ;; Specify the Display Format for Month and Day on Anniversaries
      ;; (setq ebdb-anniversary-md-format "%B %d")
      ;; Specify the Display Format for Year, Month and Day on Anniversaries
      ;; (setq ebdb-anniversary-ymd-format "%B %d, %Y")

      ;; Set Keybindings
      (pet/leader-keys
	"c"  '(:ignore t :which-key "Contacts")
	"co" '(ebdb-open :which-key "Open Contact Database")

	)
      )

;; Store Location of Account Settings
(setq pet/mail-accounts-config
	      (concat pet/home-dir
			      (convert-standard-filename
			       ".config/emacs-config/MailAccounts.el")))

;; Add mu4e directory to load path
(if (file-directory-p "/usr/share/emacs/site-lisp/mu4e")
	(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
      (if (file-directory-p "/usr/local/share/emacs/site-lisp/mu/mu4e")
	      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
	nil)
      )

;; Load mu4e as a Mail Interface for mu
(use-package mu4e
      :defer 20 ; Wait until 20 seconds after startup
      :config

      ;; Load org-mode integration
      (require 'mu4e-org)

      ;; Refresh mail using isync/mbsync every 10 minutes
      (setq mu4e-update-interval (* 10 60))
      (setq mu4e-get-mail-command "mbsync -a")
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
      (when
	 (file-exists-p pet/mail-accounts-config)
       (load pet/mail-accounts-config)
       )

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
	"ms" 'mu4e-update-mail-and-index)
      )

;; Sent alerts for received 
(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (when (boundp 'pet/mu4e-inbox-query-new)
    (setq mu4e-alert-interesting-mail-query
          pet/mu4e-inbox-query-new)
    )

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  ;; Display symbol for received mails on mode line
  (mu4e-alert-enable-mode-line-display)
  ;; Enalbe Notifications
  (mu4e-alert-enable-notifications))

;; Define Servers
(defun pet/irc-libera-server
	(interactive)
      (erc-tls :server "irc.libera.chat"
		       :port   "6697")
      )
(defun pet/irc-hackint-server
	(interactive)
      (erc-tls :server "irc.hackint.org"
		       :port   "6697")
      )
(defun pet/irc-hackint-de-server
	(interactive)
      (erc-tls :server "irc.hackint.org"
		       :port   "6697")
      )
(defun pet/irc-oftc-server
	(interactive)
      (erc-tls :server "irc.oftc.net"
		       :port   "6697")
      )

;; Setup ERC Chat Client
;; Set the Prompt to represent the the buffer-name 
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))

	      ;; Basic Account Config
	      ;; Default Server
	      erc-server "irc.libera.chat"
	      erc-nick "sailti"

	      ;; More info on the modeline
	      erc-track-shorten-start 8

	      ;; cleanup buffers
	      erc-kill-buffer-on-part t

	      ;; channel list
	      erc-autojoin-channel-alist
	      '(("irc.libera.chat"
		 "#systemcrafters"
		 "#emacs"))

	      ;; bury private messages in buffer list
	      erc-auto-query 'bury

	      ;; Autofill nickname column to 20 chars for better formatting 
	      erc-fill-function 'erc-fill-static
	      erc-fill-static-center 30
	      )

;; Set Keyboard to be accessable by 'C-c i' 
(global-set-key (kbd "C-c i") 'erc-tls)

(pet/leader-keys
      "i"  '(:ignore t :which-key "IRC")
      "mi" 'erc-tls
      "ml" '(pet/irc-libera-server :which-key "Libera Chat")
      "mh" '(pet/irc-hackint-server :which-key "Hack Int")
      "mo" '(pet/irc-oftc-server :which-key "Open and Free Technology Community")
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

;; Add Ledger Mode from Melpa
;; (Alternatively include the installation path of ledger to load-path)
(use-package ledger-mode
  :config
  ;; Add mode Toggle to Keyspace
  (pet/leader-keys
    "tml"   '(ledger-mode :which-key "Ledger Mode"))

  ;; Load mode on .dat files
  :mode "\\.dat\\'")

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
      :ensure org-plus-contrib
      :bind (("C-c l" . org-store-link))
      :config
      ;; Add additional Export Options
      (require 'ox-beamer)       ;; LaTeX beamer
      (require 'ox-koma-letter)  ;; LaTeX KOMA Script
      (require 'ox-md)           ;; Markdown
      (require 'ox-texinfo)      ;; Texinfo
      (require 'ox-man)          ;; Man Page
      (require 'ox-org)          ;; Org Format

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
	      (sequence "MEET(m)")
	      ;; Sequence 3
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
	'(
	      (nil :maxlevel . 9)
	      ("~/Org/personal-archive.org" :maxlevel . 1)
	      ("~/Org/personal-tasks.org" :maxlevel . 1)
	      ("~/Org/personal-sources.org" :maxlevel . 1)
	      ("~/Backup/Web-Bookmarks/1-bookmarks-import.org" :maxlevel . 9)
	      ("~/Backup/Web-Bookmarks/2-bookmarks-export.org" :maxlevel . 9)
	      ))

      ;; Allow Creation of Parent nodes but ask for confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      ;; The default here is 999, which is a little to constricting for SQL and such
      (setq org-table-convert-region-max-lines 9999)

      ;; Save Org buffers after refiling!
      (advice-add 'org-refile :after 'org-save-all-org-buffers)

      (pet/leader-keys
	"ot" '(:ignore t :which-key "Toggle")
	"otb" '(pet/org-toggle-babel-confirm-evaluate
		:which-key "Babel Confirm Evaluation")
	"otc" '(org-cdlatex-mode
		:which-key "Org CDLaTeX Minor Mode")
	"oti" '(org-toggle-inline-images
		:which-key "Inline Images")
	"otp" '(org-toggle-pretty-entities
		:which-key "Pretty entities")
	"oi" '(:ignore t :which-key "Import")
	"oit" '(org-table-import
		:which-key "Table")
	"oa"  '(org-agenda :which-key "Org Agenda")
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
				 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
			       ))

(with-eval-after-load 'ox-koma-letter
      '(progn
	 (add-to-list 'org-latex-classes
				      '("scrlttr2"
					"\\documentclass\{scrlttr2\}
       \[DEFAULT-PACKAGES]
       \[PACKAGES]
       \[EXTRA]"))
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
	 ))

;; Bigger LaTeX Previews
(plist-put org-format-latex-options :scale 1.5)
;; Load language packages for pdflatex of lualatex / xelatex compilers
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "babel" t ("pdflatex")))
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

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

    (perl . t)          ;; Perl
    ;; (php . t)           ;; PHP
    (R . t)             ;; R
    ;; (Ruby . t)          ;; Ruby
    (lua . t)           ;; Lua Programming Language
    (shell . t)         ;; Command Line Programs 
    (latex . t)         ;; LaTeX  
    (sql . t)           ;; SQL
    (sqlite . t)        ;; SQLite
    (octave . t)        ;; Octave
    (gnuplot . t)       ;; Gnuplot
    (awk . t)           ;; awk
    (sed . t)           ;; GNUsed
    (css . t)           ;; CSS
    (plantuml . t)      ;; PlantUML
    ;; (ledger . t)        ;; Ledger CLI
    ))         

;; Add conf-unix to be recognized
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

;; Setup Source Block Templates
(cl-loop for block in
		 '(("aw" . "src awk")
		       ("el" . "src emacs-lisp")
		       ;; ("go" . "src go")
		       ;; ("ip" . "src ipython :session :async :exports both :results raw drawer")
		       ("json" . "src json")
		       ("lua" . "src lua")
		       ("ldg" . "src ledger :no")
		       ("oc" . "src octave")
		       ("perl" . "src perl")
		       ("ph" . "src php")
		       ("py" . "src python")
		       ("sc" . "src scheme")
		       ("sh" . "src shell")
		       ("sq" . "src sql")
		       ("yaml" . "src yaml")
		       ;; R
		       ("rp" . "src R")                                  ;; pure R             
		       ("rr" . "src R :results both output")             ;; R with output
		       ("rs" . "src R :session :results both output")    ;; R with output
		       ;; ("ts" . "src typescript"))
		       ;; This is an alternative Block
		       ;; For IPython
		       ;; ("si" . "src ipython :session :async :results output")
		       )
		 do
		 (add-to-list
		      'org-structure-template-alist block))

;; Org-Capture
(use-package org-capture
  :straight nil
  :init

  ;; Org Capture helper Variables

  ;; Save default Org Capture Template Dir
  (setq pet/org-cap-temp-dir
        (concat pet/temp-dir
                (convert-standard-filename
                 "X1_Emacs_Templates/Org_Capture_Templates/"
                 )))

  ;; Org Capture helper Function

  ;; Get filename and store at current directory
  (defun pet/get-file-name-without-extension ()
    "Ask user for filename"
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name name))
    )
  ;; Get filename with ".org" and store at current directory
  (defun pet/get-org-file-name ()
    "Ask user for filename"
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name
       (format "%s.org" name)))
    )

  :config
  ;; Set default file for capture if not specified
  (setq org-default-notes-file
        (concat pet/home-dir "Projects/Notes/notes.org"))

  (setq org-capture-templates
        ;; Acronym captures
        `(("a" "Acronyms" table-line
           (file+headline "~/Org/acronyms.org" "Inbox")
           "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

          ;; Documents
          ("d" "Documents")
          ("dl" "Letter")
          ("dlf" "Letter Form" plain (file pet/get-org-file-name)
           "%[~/.dotfiles/00_OrgFiles/Templates/Capture-LetterTemp.org]"
           :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t
           )
          ("dlh" "Letter Home" plain (file pet/get-org-file-name)
           "%[~/Templates/X1_Emacs_Templates/Capture-LetterTemp-Filled-Home-Real.org]"
           :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t
           )
          ("do" "Org File")
          ("dod" "Default Org File"
           plain
           (file pet/get-org-writing-file-name)
           "#+TITLE: ${title}\n%[~/Templates/X1_Emacs_Templates/Org_Capture_Templates/default-org-file.orgctemp]"
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

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

;; (setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

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

;; Org AddOn Auto Tangle Org Files
;; Add '#+auto_tangle: t' to files 
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; Add rainbow delimiters for better readability
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Customize highlighting of matching parenthesis
(use-package paren
:config
(set-face-attribute
 'show-paren-match-expression nil :background "#363e4a")
(show-paren-mode 1))

;; Helper Functions (for hooks mostly)
(defun pet/company-text-mode ()
  "Sets 'company-mode' for 'text-mode'"
  ;; Activate completion after 3 letters in text mode
  (setq company-minimum-prefix-length 3))

;; Load Company Mode for Auto Completion
(use-package company
  :config
  ;; activate global company mode
  (global-company-mode 1)
  ;; make company perform completions with tab
  (company-tng-mode 1)

  ;; Set hooks
  (add-hook 'text-mode-hook 'pet/company-text-mode)

  ;; Enable Cycling Options back to the Beginning
  (setq company-selection-wrap-around t)

  ;; Align Annotations (paramenters, arguments, etc..) right of tooltip
  (setq company-tooltip-align-annotations t)

  ;; Disable Company Mode in Strings or Comment
  (setq company-idle-delay
    (lambda () (if (company-in-string-or-comment) nil 0.3)))

  ;; Allow Prefix Length to be change per buffer
  (make-variable-buffer-local 'company-minimum-prefix-length)

  ;; Add Company Mode to Leader Keys
  (pet/leader-keys
    "tmc"  '(global-company-mode :which-key "Global Company Mode")
    )
  )

;; Add Company Extension for Bash and Shell
(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env))
  )

;; local configuration for TeX modes
(defun pet/company-latex-mode ()
  "Sets 'company-mode' for 'text-mode'"
  ;; Add Backands
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

;; Add Company Extension for LaTeX Math
(use-package company-math
  :config

  ;; Add hooks to Modes
  ;; Tex Mode
  (add-hook 'tex-mode-hook 'pet/company-latex-mode)
  ;; Org Mode
  (add-hook 'org-mode-hook 'pet/company-latex-mode)

  ;; global activation of the unicode symbol completion 
  ;;(add-to-list 'company-backends 'company-math-symbols-unicode))
  )

;; Add Company Extension for C/C++
;; (use-package company-c-headers)

;; local configuration for Python modes
(defun pet/company-python-mode ()
  "Sets 'company-mode' for 'text-mode'"
  ;; Activate completion after 1 letters in python mode
  (setq company-minimum-prefix-length 1)
  ;; Add Jedi to Company Backends
  (add-to-list 'company-backends 'company-jedi))


;; Add Company Extension for Python
(use-package company-jedi
  :config
  (add-hook 'python-mode-hook 'pet/company-python-mode)
)

;; local configuration for Python modes
(defun pet/company-lua-mode ()
  "Sets 'company-mode' for 'text-mode'"
  ;; Activate completion after 1 letters in python mode
  (setq company-minimum-prefix-length 1)
  ;; Add Lua to Company Backends
  (setq-local company-backends '((company-lua
                                  company-etags
                                  company-dabbrev-code
                                  company-yasnippet))))


  ;; Add Company Extension for Lua
  (use-package company-lua
    :config
    (add-hook 'lua-mode-hook 'pet/company-lua-mode)
  )

;; Add Alternative Frontend
(use-package company-box
  :hook (company-mode . company-box-mode))

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
      ("M-TAB" . completion-at-point))
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
    ;; "tln" 'lsp-ui-find-next-reference
    ;; "tlp" 'lsp-ui-find-prev-reference
    "tls" 'counsel-imenu
    ;; "tle" 'lsp-ui-flycheck-list
    ;; "tlS" 'lsp-ui-sideline-mode
    "tlX" 'lsp-execute-code-action)
  )

;; Add lsp ui for higher level ui options
;;(use-package lsp-ui
;;  :commands lsp-ui-mode
;;  ;; :hook (lsp-mode . lsp-ui-mode)
;;  ;; Show lsp info on sideline
;;  :config
;;  (setq lsp-ui-doc-enable nil)
;;  (setq lsp-ui-doc-header t)
;;  (setq lsp-ui-doc-include-signature t)
;;  (setq lsp-ui-doc-border (face-foreground 'default))
;;  (setq lsp-ui-sideline-show-code-actions t)
;;  (setq lsp-ui-sideline-delay 0.05)
;;  ;; (setq lsp-ui-sideline-enable t)
;;  ;; (setq lsp-ui-sideline-show-hover nil)
;;  ;; (setq lsp-ui-doc-position 'bottom)
;;  )

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
      :straight auctex
      :config
      ;; Add Reftex Support to AUCTeX
      (setq reftex-plug-into-AUCTeX t)
      ;; Set Default Bibliography
      (setq pet/default-bib
		(concat pet/home-dir "~/Projects/Writing/00_Bibliographies/Main_Bib.bib"))
      (setq reftex-default-bibliography '("~/Projects/Writing/00_Bibliographies/Main_Bib.bib"))
      ;; Automatically insert math environment with '$'
      (setq TeX-electric-math t)
      ;; Autocomplete command on '\'
      (setq TeX-electric-escape t)
      ;; Autoinsert braces after '^' and '_' in math mode
      (setq TeX-electric-sub-and-superscript t)
      )


;; enable auto saving tex files
(setq TeX-auto-save t)
;; enable completion and multifile structure (include/input)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; set $ to insert math environment
;; ... for plain TeX
(add-hook 'plain-TeX-mode-hook
		      (lambda () (set (make-local-variable 'TeX-electric-math)
						      (cons "$" "$"))))
;; ... for LaTeX
(add-hook 'LaTeX-mode-hook
		      (lambda () (set (make-local-variable 'TeX-electric-math)
						      (cons "\\(" "\\)"))))

;; Load RefTeX...
;; ... with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; ... with with Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; Enable auto completion of right braces (")}]\)\]\}")
;; Use 'C-u 1' or 'C-q' before to disable 
(setq LaTeX-electric-left-right-brace t)

;; LatexMK support for AUCTeX
;; (use-package auctex-latexmk)

;; Useful features for LaTeX-mode
;;(use-package latex-extra)

;; Fast input methods for LaTeX environments and math
(use-package cdlatex
      :bind
      (:map LaTeX-mode-map
		("C-#" . cdlatex-mode))
      :config
      ;; Maybe add hook to autoload cdlatex
      ;; (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
      ;; (add-hook 'org-mode-hook #'turn-on-org-cdlatex)

      ;; Added personal keybinding
      (pet/leader-keys
		"tmc" '(cdlatex-mode
				:which-key "CDLaTeX Minor Mode"))
      )

(setq TeX-view-program-selection
	      '(((output-dvi has-no-display-manager) "dvi2tty")
		((output-dvi style-pstricks) "dvips and gv")
		(output-dvi "xdvi")
		(output-pdf "Zathura")
		(output-html "xdg-open")))

;; Set Default Indentation for Python 
(setq-default python-indent-offset 4)

      ;;;; Customize Python Mode for emacs, add lsp
;;(add-hook 'python-mode-hook 'lsp-deferred)
;;  :custom
;;  (python-shell-interpreter "python")
;;  (dab-python-executable "python")
;;  (dab-python-debugger 'debugpy)
;;  :config
;;  (require 'dab-python)
;;  )

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

;; Load PHP Package
(use-package php-mode)

;; Load Org PHP Support
;; (use-package ob-php)

;; Add Mode for Lua
(use-package lua-mode)

;; Add alternative Mode for HTML Developement
;; (use-package web-mode)

;; Load Support for statistical Computation
(use-package ess)

;; Helper Package for assignment operators and more in ESS
(use-package ess-smart-equals
  ;; Add additional support (automatic pared braces etc..)
  :init   (setq ess-smart-equals-extra-ops '(brace paren percent))
  ;; Load moade with ESS
  :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  ;; Activate modoe
  :config (ess-smart-equals-activate))

;; improve interaction between ESS and R Package 'tidyverse'
(use-package ess-r-insert-obj)

;; Tidyverse-like data views and manipulations
(use-package ess-view-data)

;; Major mode for editing comma/char separated values
(use-package csv-mode)

;; Add support for YAML files
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Add PlantUML Support
(use-package plantuml-mode
  :config
  ;; Set Execution Mode to Render with Local Binary
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; Set load path condition
  :mode "\\.pl?a?n?t?uml\\'"
  )
