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
      (setq reftex-default-bibliography
		(list
		 pet/main-bib))
      ;; Automatically insert math environment with '$'
      (setq TeX-electric-math t)
      ;; Autocomplete command on '\'
      (setq TeX-electric-escape t)
      ;; Autoinsert braces after '^' and '_' in math mode
      (setq TeX-electric-sub-and-superscript t)
      )

;; Specify viewer programs
(setq TeX-view-program-selection
	      '(((output-dvi has-no-display-manager) "dvi2tty")
		((output-dvi style-pstricks) "dvips and gv")
		(output-dvi "xdvi")
		(output-pdf "Okular")
		(output-pdf "PDF Tools")
		;; (output-pdf "Evince")
		(output-html "xdg-open")))


;; Specify Viewer Calls
(setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

;; Activate TeX source correlate Mode
(setq TeX-source-correlate-mode t)
;; Starts server for inverse search
(setq TeX-source-correlate-start-server t)

;; Hook to automatically refresh output buffer
(add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)

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

;; Activate source correlate mode
(add-hook 'plain-TeX-mode-hook
		      (lambda () (setq TeX-source-correlate-mode t)))

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
				:which-key "CDLaTeX Minor Mode")))

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

;; Enable Virtual Environment Support
(use-package pyvenv
      :config
      (pyvenv-mode 1))

;; Add Support for Rust
(use-package rust-mode)

;; Load PHP Package
(use-package php-mode)

;; Load Org PHP Support
;; (use-package ob-php)

;; Add Mode for Lua
(use-package lua-mode)

;; Add Support for the Julia Programming Language
(use-package julia-mode)

;; Add Support for Nix Language and Nix Configurations
(use-package nix-mode
      :mode ("\\.nix\\'" "\\.nix.in\\'"))
;; Viewing Nix .drv Files
(use-package nix-drv-mode
      :straight nil ;; already loaded with nix-mode
      :ensure nix-mode
      :mode "\\.drv\\'")
;; Viewing Nix .drv Files
(use-package nix-flake
      :straight nil ;; already loaded with nix-mode
      :ensure nix-mode)
;; Provides Interactive Shell to call nix functions
(use-package nix-shell
      :straight nil ;; already loaded with nix-mode
      :ensure nix-mode
      :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
;; Adds a REPL for Nix (plus interface for company)
(use-package nix-repl
      :straight nil ;; already loaded with nix-mode
      :ensure nix-mode
      :commands (nix-repl))

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

;; Add Gnuplot Support
(use-package gnuplot-mode
      :config
      ;; Use this if gnuplot is not /usr/bin/gnuplot
      (setq gnuplot-program "/usr/bin/gnuplot")

      ;; automatically enter gnuplot mode
      (setq auto-mode-alist 
		(append '(("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)) auto-mode-alist))
      )

;; Add PlantUML Support
(use-package plantuml-mode
  :config
  ;; Set Execution Mode to Render with Local Binary
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; Set load path condition
  :mode "\\.pl?a?n?t?uml\\'")
