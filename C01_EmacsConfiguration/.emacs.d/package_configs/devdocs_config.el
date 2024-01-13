(use-package devdocs
  :defer
  :bind ("C-c M-d" . pet/devdocs-lookup)
  :init
  (add-to-list 'display-buffer-alist
               '("\\*devdocs\\*"
                 display-buffer-in-side-window
                 (side . right)
                 (slot . 3)
                 (window-parameters . ((no-delete-other-windows . t)))
                 (dedicated . t)))

  ;; Specify the directory to install devdocs to -- default is ’~/.emacs.d/devdocs’
  (setq devdocs-data-dir (concat pet/dotfiles-emacsconfig-dir "devdocs"))

  (defun pet/devdocs-lookup (&optional ask-docs)
    "Light wrapper around `devdocs-lookup` which pre-populates the function input with thing at point"
    (interactive "P")
    (let ((query (thing-at-point 'symbol t)))
      (devdocs-lookup ask-docs query)))

  ;; Narrow down the options by setting up hooks for language modes
  :hook ((python-mode . (lambda () (setq-local devdocs-current-docs
                                      '("django~4.2" "django_rest_framework" "python~3.12" "postgresql~16" "sqlite"))))
         (web-mode . (lambda () (setq-local devdocs-current-docs '("vue~3"
                                                                   "vue_router~4"
                                                                   "javascript"
                                                                   "typescript"
                                                                   "vitest"
                                                                   "moment"
                                                                   "tailwindcss"
                                                                   "html"
                                                                   "css"))))
         (typescript-mode . (lambda () (setq-local devdocs-current-docs '("vue~3"
                                                                          "vue_router~4"
                                                                          "javascript"
                                                                          "typescript"
                                                                          "vitest"
                                                                          "moment"))))))
