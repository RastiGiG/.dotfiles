;; Helper Functions (for hooks mostly)
(defun pet/company-text-mode ()
  "Sets 'company-mode' for 'text-mode'"
  ;; Activate completion after 3 letters in text mode
  (setq company-minimum-prefix-length 3))

;; Load Company Mode for Auto Completion
(use-package company
  :defer 2
  :diminish
  :init
  (setq company-begin-commands '(self-insert-command))
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)   ;; Show numbers in tooltip
  (global-company-mode 1)         ;; activate company mode
  (company-tng-mode 1)            ;; make company perform completions with tab

  ;; Disable Company Mode in Strings or Comment
  (setq company-idle-delay
    (lambda () (if (company-in-string-or-comment) nil 0.3)))

  ;; Set hooks
  (add-hook 'text-mode-hook 'pet/company-text-mode)

  ;; Enable Cycling Options back to the Beginning
  (setq company-selection-wrap-around t)

  ;; Align Annotations (paramenters, arguments, etc..) right of tooltip
  (setq company-tooltip-align-annotations t)

  ;; Allow Prefix Length to be change per buffer
  (make-variable-buffer-local 'company-minimum-prefix-length)

  ;; Add Company Mode to Leader Keys
  (pet/leader-keys
    "tmc"  '(global-company-mode :which-key "Global Company Mode")))

;; Add Company Extension for Bash and Shell
(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

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
  (add-to-list 'company-backends 'company-math-symbols-unicode))

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
  (add-hook 'python-mode-hook 'pet/company-python-mode))

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
    (add-hook 'lua-mode-hook 'pet/company-lua-mode))

;; Add Alternative Frontend
(use-package company-box
  :after company
  :diminish        ;; Don’t show mode in minibuffer
  :hook (company-mode . company-box-mode))
