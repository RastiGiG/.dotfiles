;; Add Perspective to use sets of
(use-package perspective
  :demand t
  ;; Setup Keybindings
  ;; :bind (("C-M-k" . persp-switch)
  ;; 	   ("C-M-n" . persp-next)
  ;; 	   ("C-x k" . persp-kill-buffer*))
  :init
  (persp-mode)
  :custom
  ;; Start Perspective Mode
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "Main")
  ;; Set default file for states
  (persp-state-default-file
   (concat pet/dotfiles-emacsconfig-dir
             "perspective/default-state"))
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
      (persp-mode))

  ;; Automatically save perspective states to file when Emacs exits.
  (add-hook 'kill-emacs-hook #'persp-state-save)


  ;; Add Perspective Functions to User Leader Keys
  (pet/leader-keys
      "P"  '(:ignore t :which-key "Perspectives")
      "Pn"  'persp-next
      "Ps"  'persp-switch-to-buffer*
      "Pk"  'persp-kill-buffer*))
