;; Add Language Server Support
(use-package lsp-mode
  ;; :hook ((c-mode          ;; clangd
  ;;         c++-mode        ;; clangd
  ;;         c-or-c++-mode   ;; clangd
  ;;         python-mode     ;; pyright
  ;;         typescript-mode ;; ts-ls (tsserver wrapper)
  ;;         js-mode         ;; ts-ls (tsserver wrapper)
  ;;         web-mode        ;; ts-ls/HTML/CSS
  ;;         ) . lsp-deferred)
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

;; Extend lsp and treemacs integration
(use-package lsp-treemacs
  :after (lsp treemacs))

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

;; Setup lsp-pyright Server
(use-package lsp-pyright
      :hook (python-mode . (lambda () (require 'lsp-pyright)))
      ;; Use Python 3 in case Python 2 is installed as well
      :init (when (executable-find "python3")
		      (setq lsp-pyright-python-executable-cmd "python3"))
      )

;; Lsp-Mode Settings for LaTeX 
(setq lsp-tex-server 'digestif) ;; Use Digestif
;; (setq lsp-tex-server 'texlab) ;; Use texlab instead
