;; __     ___    ____  ___    _    ____  _     _____ ____  
;; \ \   / / \  |  _ \|_ _|  / \  | __ )| |   | ____/ ___| 
;;  \ \ / / _ \ | |_) || |  / _ \ |  _ \| |   |  _| \___ \ 
;;   \ V / ___ \|  _ < | | / ___ \| |_) | |___| |___ ___) |
;;    \_/_/   \_\_| \_\___/_/   \_\____/|_____|_____|____/ 
;;

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

;; Store Org Directory
(setq pet/org-dir
	      (concat pet/home-dir
			      (convert-standard-filename
			       "Org/")))

;; Save Emacs Template Dir for later use
(setq pet/latex-header-temp-dir
	      (concat pet/temp-dir
			      (convert-standard-filename
			       "X2_LaTeX_Templates/00-Headers/")))

;; Save Path to main Bibliography File
(setq pet/bibliographies
	      (concat pet/home-dir
			      "Projects/Writing/00_Bibliographies"))

;; Save path to Emacs Configuration
(setq pet/dotfiles-emacsconfig-dir
	(concat pet/dotfiles-dir
		(convert-standard-filename
		 "C01_EmacsConfiguration/")))

;; Adjust font size to match your system
(defvar pet/default-font-size 140)
(defvar pet/default-variable-font-size 120)

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
