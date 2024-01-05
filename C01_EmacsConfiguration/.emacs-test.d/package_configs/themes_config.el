;; Load Doom Themes
(use-package doom-themes
      :init (load-theme 'doom-dracula t)
      :config
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  		doom-themes-enable-italic t) ; if nil, italics is universally disabled
      ;; Enable custom neotree theme (all-the-icons must be installed!)
      (doom-themes-neotree-config)
      ;; Corrects (and improves) org-mode's native fontification.
      (doom-themes-org-config))
