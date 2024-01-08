;; Load TOTP Functionality
(straight-use-package
 '(emacs-totp :type git :host codeberg :repo "RastiGiG/emacs-totp"))

(pet/load-file "emacs-totp.el")
