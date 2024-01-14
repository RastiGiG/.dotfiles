;;  Add Git Timemachine
(use-package git-timemachine
  :after (git-timemachine evil)
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)

      (pet/leader-keys
  	"g t" '(git-timemachine :wk "Git time machine")))

(use-package hl-todo
  :hook (;; (org-mode . hl-todo-mode)  ;; Not needed with Org Modern
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; Add Citar completion buffer for citations
(use-package citar
  ;; The `:straight' keyword is not necessary. However, I do this to set a value
  ;; for the `:includes' keyword. This keyword tells use-package that those
  ;; package(s) are provided by this package, and not to search for them on
  ;; Melpa for download. Alternatively, you can set the `:straight' keyword to
  ;; nil in those package(s) use-package declaration.
  :straight (citar :type git :host github :repo "emacs-citar/citar" :includes (citar-org))
  :custom
  ;; A list of bib files. A good idea would be having its value be identical to
  ;; that of `org-cite-global-bibliography'. For me, I have all my bib file(s)
  ;; as a list of strings in `kb/bib-files'.
  (citar-bibliography pet/main-bib)

  ;; List of directories for reference nodes
  (citar-notes-paths (list pet/org-dir))

  (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
  ;; (citar-at-point-function 'embark-act)           ; If use `embark'

  ;; Add Keybindings
  :general
  (:keymaps 'org-mode-map
  		      :prefix "C-c b"
  		      "b" '(citar-insert-citation :wk "Insert citation")
  		      "r" '(citar-insert-reference :wk "Insert reference")
  		      "o" '(citar-open-notes :wk "Open note")))

;; Use `citar' with `org-cite'
(use-package citar-org
      :after oc
      :custom
      (org-cite-insert-processor 'citar)
      (org-cite-follow-processor 'citar)
      (org-cite-activate-processor 'citar))
