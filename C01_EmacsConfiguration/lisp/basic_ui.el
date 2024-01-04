;;  ____    _    ____ ___ ____   _   _ ___    ____ ___  _   _ _____ ___ ____ 
;; | __ )  / \  / ___|_ _/ ___| | | | |_ _|  / ___/ _ \| \ | |  ___|_ _/ ___|
;; |  _ \ / _ \ \___ \| | |     | | | || |  | |  | | | |  \| | |_   | | |  _ 
;; | |_) / ___ \ ___) | | |___  | |_| || |  | |__| |_| | |\  |  _|  | | |_| |
;; |____/_/   \_\____/___\____|  \___/|___|  \____\___/|_| \_|_|   |___\____|
;;

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
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Enable Highlight-Line
(hl-line-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode-hook
		treemacs-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
