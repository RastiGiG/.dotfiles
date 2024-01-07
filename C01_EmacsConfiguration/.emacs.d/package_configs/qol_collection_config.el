;; Package to setup Path Variable (and more) in Emacs
(use-package exec-path-from-shell
  :config
  ;; Load binary directories like =.cargo/bin/= and =.local/bin= on Linux and OS X
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

;; ;; Read Path from Shell Setup when Emacs Server is launched through SystemD
;; (when (daemonp)
;;   (exec-path-from-shell-initialize))

;; Copy values of other Environment Variables
;; (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;   (add-to-list 'exec-path-from-shell-variables var))

;; Easier Commenting, not just for evil-mode
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Add TLDR Version of Man Pageship
(use-package tldr)

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

(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  (evil-define-key 'visual drag-stuff-mode-map (kbd "K") 'drag-stuff-up)
  (evil-define-key 'visual drag-stuff-mode-map (kbd "J") 'drag-stuff-down)
  (evil-define-key 'visual drag-stuff-mode-map (kbd "H") 'drag-stuff-left)
  (evil-define-key 'visual drag-stuff-mode-map (kbd "L") 'drag-stuff-right))
