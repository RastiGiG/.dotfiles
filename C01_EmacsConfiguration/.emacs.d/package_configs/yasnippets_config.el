;; Yasnippets
(use-package yasnippet
  :init
  ;; save Yasnippet dir
  (setq pet/yasnippet-dir
        (concat pet/dotfiles-emacsconfig-dir
                "snippets"))

  :config
  ;; Set Yasnippet dir
  (setq yas-snippet-dirs '(pet/yasnippet-dir))

  ;; Activate Yasnippets globally
  (yas-global-mode 1)

  ;; Allow Stacked Expansion (Expansion within Expansion)
  ;; (setq yas-triggers-in-field t)

  ;; Enable snippets being shared between modes
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode
               'fundamental-mode))))

;; Add some predefined snippets
(use-package yasnippet-snippets
  :defer)
