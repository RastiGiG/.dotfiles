;;  ____    _    ____ ___ ____   _   _ ___    ____ ___  _   _ _____ ___ ____
;; | __ )  / \  / ___|_ _/ ___| | | | |_ _|  / ___/ _ \| \ | |  ___|_ _/ ___|
;; |  _ \ / _ \ \___ \| | |     | | | || |  | |  | | | |  \| | |_   | | |  _
;; | |_) / ___ \ ___) | | |___  | |_| || |  | |__| |_| | |\  |  _|  | | |_| |
;; |____/_/   \_\____/___\____|  \___/|___|  \____\___/|_| \_|_|   |___\____|
;;

;; Start Emacs in Fullscreen mode and set transparancy
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Set Column Numbers
(column-number-mode)
;; Set Line Numbers Globally
(global-display-line-numbers-mode t)
;; Set default line number width
(setq display-line-numbers-width 4)
;; Adjust Line Numbers Width
(setq display-line-numbers-widen t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
  			prog-mode-hook
  			conf-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 'relative))))

;; Disable line numbers for some modes
(dolist (mode
         '(;;org-mode-hook
  	     term-mode-hook
  	     vterm-mode-hook
  	     shell-mode-hook
  	     eshell-mode-hook
  	     pdf-view-mode-hook
  	     treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set the amount of space before lines get wrapped (default 70)
(setq fill-column 80)

;; Set up the visible bell
(setq visible-bell t)

;; Enable Highlight-Line
(global-hl-line-mode 1)

;; Disable Highlight-line for some modes
(dolist (mode
         '(;;org-mode-hook
  	     term-mode-hook
  	     vterm-mode-hook
  	     shell-mode-hook
  	     eshell-mode-hook
  	     pdf-view-mode-hook))
  (add-hook mode (lambda () (hl-line-mode nil))))

;; Add a little space when displaying buffers
(setq-default line-spacing 0.12)

;; Set default Encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Display battery for when in full screen mode
(display-battery-mode t)

;; Display clock in modeline
(display-time-mode 1)

;; Calendar week starts on Monday (1)
;; default: Sunday (0)
(setq calendar-week-start-day 1)

;; Display Week Numbers in Calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0 :foreground "salmon") ;; Week Numbers display in different Color
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; Display Header for Week Numbers in Calendar
(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 1.0)
(setq calendar-intermonth-header
      (propertize "KW"                  ; or e.g. "Wk" in English
                  'font-lock-face 'calendar-iso-week-header-face))

; Number of side window slots at
; left - top - right - bottom
(setq window-sides-slots '(3 0 3 1))

;; No popup windows
(setq pop-up-windows nil)

;; Enable Winner Mode
(winner-mode 1)

;; Set default font face of present
(when (pet/font-available-p "Iosevka")
	      (set-face-attribute 'default nil :font "Iosevka"
						      :height pet/default-font-size))

;; Set the fixed pitch face
(when (pet/font-available-p "Iosevka")
	      (set-face-attribute 'fixed-pitch nil :font "Iosevka"
						      :height pet/default-font-size))

;; Set the variable pitch face
(when (pet/font-available-p "Cantarell")
	      (set-face-attribute 'variable-pitch nil :font "Cantarell"
						      :height pet/default-font-size
						      :weight 'regular))

;; (when (member "Cantarell" (font-family-list))
;; 	  (set-face-attribute 'variable-pitch nil :font "Cantarell"
;; 						  :height pet/default-font-size
						      ;; :weight 'regular))

;; Use specific Fontsets for Symbols
(setq use-default-font-for-symbols nil)

;; Use Symbols Nerd Font as Default Symbols Font, otherwise fall back to Symbola (or else)
(set-fontset-font t 'unicode "Symbols Nerd Font")
(set-fontset-font t '(#xF500 . #xF8FF) "Symbols Nerd Font")
(set-fontset-font t 'unicode "Symbola" nil 'append)
(set-fontset-font t 'unicode (font-spec :script 'unicode) nil 'append)

;; Configuring Dired

;; The prefixes are arguments given to "ls" by dired
(setq dired-listing-switches
       "-aghlv --group-directories-first")

;; Set dired jump keymap
(global-set-key (kbd "C-x C-j") 'dired-jump)

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

;; Pop up buffer in new window, when calling switch to buffer in dedicated windows
(setq switch-to-buffer-in-dedicated-window t)

;; Show help buffers in same window
(add-to-list 'display-buffer-alist
   		       '("\\*Help\\*"
   			 (display-buffer-reuse-window display-buffer-pop-up-window)
   			 (inhibit-same-window . t)))

;; Show info buffers in side window
(add-to-list 'display-buffer-alist
   		       '("\\*info\\*"
   			 (display-buffer-in-side-window)
   			 (side . right)
   			 (slot . 0)
   			 (window-width . 80)
   			 (window-parameters
   			      (no-delete-other-windows . t))))

;; Compilation windows should reuse the same window
(add-to-list 'display-buffer-alist
   		       '("\\*Compilation\\*"
   			 display-buffer-reuse-window))

;; Make spezific modes reuse the same window
(add-to-list 'display-buffer-alist
  		       `(,(rx (| "*xref*"
  					 "*grep*"
  					 "*Occur*"))
  			 display-buffer-reuse-window
  			 (inhibit-same-window . nil)))
