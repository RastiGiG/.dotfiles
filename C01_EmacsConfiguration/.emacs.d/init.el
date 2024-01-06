;;   _____ __  __    _    ____ ____  
;;  | ____|  \/  |  / \  / ___/ ___| 
;;  |  _| | |\/| | / _ \| |   \___ \ 
;;  | |___| |  | |/ ___ \ |___ ___) |
;;  |_____|_|  |_/_/   \_\____|____/ 
;;                                   

;; NOTE: init.el is generated from EmacsTestConfig.org.
;; Please change your settings in that file
;; using Emacs and init.el will be generated automatically!

;; bootstrap script to install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;; Early load Org Mode
(use-package org)

;; Set mu4e directory path
(if (file-directory-p "~/Projects/Programs/github-gitlab/mu/build/mu4e")
	(setq pet/mu4e-load-path "~/Projects/Programs/github-gitlab/mu/build/mu4e")
      (if (file-directory-p "/usr/share/emacs/site-lisp/mu4e")
      (setq pet/mu4e-load-path "/usr/share/emacs/site-lisp/mu4e")
	(if (file-directory-p "/usr/share/emacs/site-lisp/mu/mu4e")
	(setq pet/mu4e-load-path "/usr/share/emacs/site-lisp/mu/mu4e")
      (if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
	      (setq pet/mu4e-load-path "/usr/local/share/emacs/site-lisp/mu4e")
	(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu/mu4e")
		(setq pet/mu4e-load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
	      nil)))))

;; Add mu4e load path
(if (boundp 'pet/mu4e-load-path)
	(add-to-list 'load-path pet/mu4e-load-path)
      nil)

;; Load Functions
(when (file-exists-p "~/.dotfiles/C01_EmacsConfiguration/lisp/functions.el")
  (load "~/.dotfiles/C01_EmacsConfiguration/lisp/functions.el"))

;; Load Variables
(pet/load-file "~/.dotfiles/C01_EmacsConfiguration/lisp/variables.el")

;; Load Basic UI
(pet/load-file (concat pet/dotfiles-emacsconfig-dir "lisp/basic_ui.el"))

;; Load Basic QoL Adjustments
(pet/load-file (concat pet/dotfiles-emacsconfig-dir "lisp/basic_qol.el"))

;; Load Evil Configuration
(pet/load-file (concat user-emacs-directory "package_configs/evil_config.el"))

;; Load General Configuration adjusted for Evil
(pet/load-file (concat user-emacs-directory "package_configs/general_evil_config.el"))
;; Load General Leader Keys after General init
(pet/load-file (concat user-emacs-directory "package_configs/general_leader_keys.el"))

;; ;; Load General Configuration for standard Emacs
;; (pet/load-file (concat user-emacs-directory "package_configs/general_standard_config.el"))
;; ;; Load General Leader Keys after General init
;; (pet/load-file (concat user-emacs-directory "package_configs/general_leader_keys.el"))

;; Load Which Key Configuration
(pet/load-file (concat user-emacs-directory "package_configs/which_key_config.el"))

;; Load Dired Extensions Configuration
(pet/load-file (concat user-emacs-directory "package_configs/dired_extensions_config.el"))

;; ;; Load Ranger Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/ranger_config.el"))

;; Load PDF-Tools Configuration
(pet/load-file (concat user-emacs-directory "package_configs/pdf_tools_config.el"))

;; Load All The Icons Configuration
(pet/load-file (concat user-emacs-directory "package_configs/all_the_icons_config.el"))

;; Load Doom Modeline Configuration
(pet/load-file (concat user-emacs-directory "package_configs/doom_modeline_config.el"))

;; Load Themes Configuration
(pet/load-file (concat user-emacs-directory "package_configs/themes_config.el"))

;; Load QoL Extensions Collection Configuration
(pet/load-file (concat user-emacs-directory "package_configs/qol_collection_config.el"))

;; ;; Load Undo-Tree Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/undo_tree_config.el"))

;; ;; Load Multiple Cursors Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/multiple_cursors_config.el"))

;; Load Yasnippets Configuration
(pet/load-file (concat user-emacs-directory "package_configs/yasnippets_config.el"))

;; Load Dashboard Configuration
(pet/load-file (concat user-emacs-directory "package_configs/dashboard_config.el"))

;; ;; Load Beacon Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/beacon_config.el"))

;; Load GUI Extensions Collection
(pet/load-file (concat user-emacs-directory "package_configs/gui_extensions_config.el"))

;; Load Ivy (and Counsel) Configuration
(pet/load-file (concat user-emacs-directory "package_configs/ivy_config.el"))

;; Load Perspective Configuration
(pet/load-file (concat user-emacs-directory "package_configs/perspective_config.el"))

;; Load Treemacs Configuration
(pet/load-file (concat user-emacs-directory "package_configs/treemacs_config.el"))

;; ;; Load Neotree Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/neotree_config.el"))

;; ;; Load Hydra Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/hydra_config.el"))

;; Load Org Configuration
(pet/load-file (concat user-emacs-directory "package_configs/org_config.el"))

;; Load Ledger Mode Configuration
(pet/load-file (concat user-emacs-directory "package_configs/ledger_mode_config.el"))

;; Load Vterm Mode Configuration
(pet/load-file (concat user-emacs-directory "package_configs/vterm_config.el"))

;; Load EShell Mode Configuration
(pet/load-file (concat user-emacs-directory "package_configs/eshell_config.el"))

;; Load Magit Configuration
(pet/load-file (concat user-emacs-directory "package_configs/magit_config.el"))

;; Load Developement Tools Collection Configuration
(pet/load-file (concat user-emacs-directory "package_configs/dev_tools_collection_config.el"))

;; ;; Load Company Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/company_config.el"))

;; Load Consult Configuration
(pet/load-file (concat user-emacs-directory "package_configs/consult_config.el"))

;; ;; Load Traditional Language Modes
;; (pet/load-file (concat user-emacs-directory "package_configs/language_modes_config.el"))

;; ;; Load Tressitter Language Modes
;; (pet/load-file (concat user-emacs-directory "package_configs/language_ts_modes_config.el"))

;; ;; Load LSP Mode Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/lsp_mode_config.el"))

;; ;; Load ERC Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/eglot_config.el"))

;; ;; Load Citeproc Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/citeproc_config.el"))

;; ;; Load mu4e Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/mu4e_config.el"))

;; ;; Load Password Store Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/password_store_config.el"))

;; ;; Load EBDB Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/ebdb_config.el"))

;; ;; Load Elfeed Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/elfeed_config.el"))

;; ;; Load ERC Configuration
;; (pet/load-file (concat user-emacs-directory "package_configs/erc_config.el"))

;; Load ERadio Configuration
(pet/load-file (concat user-emacs-directory "package_configs/eradio_config.el"))

(pet/leader-keys
      "E"   '(:ignore t :which-key "eval")
      "Eb"  '(eval-buffer :which-key "eval buffer"))

(pet/leader-keys
      :keymaps '(visual)
      "Er" '(eval-region :which-key "eval region"))
