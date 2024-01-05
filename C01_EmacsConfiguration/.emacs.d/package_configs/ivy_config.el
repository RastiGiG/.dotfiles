;; Load Ivy Completion Framework
(use-package ivy
      :diminish
      :bind (("C-s" . swiper)
  	 ("C-r" . swiper)
  	 :map ivy-minibuffer-map
  	 ("TAB" . ivy-alt-done)
  	 ("C-l" . ivy-alt-done)
  	 ("C-j" . ivy-next-line)
  	 ("C-k" . ivy-previous-line)
  	 ("C-RET" . ivy-immediate-done)
  	 :map ivy-switch-buffer-map
  	 ("C-k" . ivy-previous-line)
  	 ("C-l" . ivy-done)
  	 ("C-d" . ivy-switch-buffer-kill)
  	 :map ivy-reverse-i-search-map
  	 ("C-k" . ivy-previous-line)
  	 ("C-d" . ivy-reverse-i-search-kill))
      :config
      (ivy-mode 1))

;; Add Counsel for customized find files etc..
(use-package counsel
      :after ivy
      :bind (("C-M-j" . 'counsel-switch-buffer)
  	 :map minibuffer-local-map
  	 ("C-r" . 'counsel-minibuffer-history))
      :config
      (counsel-mode 1)

      ;; Add Counsel function to leader key space
      (pet/leader-keys
  	"r"   '(ivy-resume :which-key "ivy resume")

  	"ff"  '(counsel-find-file :which-key "open file")
  	"C-f" 'counsel-find-file
  	"fr"  '(counsel-recentf :which-key "recent files")
  	"fR"  '(revert-buffer :which-key "revert file")
  	"fj"  '(counsel-file-jump :which-key "jump to file")
  	"pf"  'counsel-projectile-find-file
  	"ps"  'counsel-projectile-switch-project
  	"pF"  'counsel-projectile-rg
  	"pp"  'counsel-projectile
  	"pd"  'projectile-dired))

;; Add Prescient for spooky Emacs Memory (history)
(use-package prescient
      :after counsel
      :config
      (prescient-persist-mode 1))

;; Enable Prescient in Ivy
(use-package ivy-prescient
      :after prescient
      :config
      (ivy-prescient-mode 1))

;; Ivy-Rich: Add Descriptions alongside M-x commands
(use-package ivy-rich
      :after ivy
      :init
      (ivy-rich-mode 1))

;; Projectile Counsel Integration
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Add BibTex completion support to Ivy
(use-package ivy-bibtex
      :config
      ;; Set Bibtex Bibliography Files
      (setq bibtex-completion-bibliography
  		(list
  		 pet/main-bib
  		 ))

      ;; Set Bibtex Completion Library Path
      (setq bibtex-completion-library-path
  		(list
  		 pet/bibliography-dir
  		 ))

      ;; Set Bibtex Completion Notes Path
      (setq bibtex-completion-notes-path
  		"Projects/bibliography/notes/")

      ;; Add Keywords Field to Completion Serach
      (setq bibtex-completion-additional-search-fields '(keywords))

      ;; ;; Bibtex Notes Completion Template
      ;; (setq bibtex-completion-notes-template-multiple-files
      ;; "* ${author-or-editor}, ${title}, ${journal}, (${year})  :${=type=}:  \n\nSee   [[cite\:${=key=}]]  \n")

      ;; Display Format for Completions
      ;; (setq bibtex-completion-display-formats
      ;;	  '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
      ;;		(inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
      ;;		(incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      ;;		(inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
      ;;		(t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))

      ;;; Adjust automatic generation of bibtex key
      ;;(setq bibtex-autokey-year-length 4
      ;;	  bibtex-autokey-name-year-separator "-"
      ;;	  bibtex-autokey-year-title-separator "-"
      ;;	  bibtex-autokey-titleword-separator "-"
      ;;	  bibtex-autokey-titlewords 2
      ;;	  bibtex-autokey-titlewords-stretch 1
      ;;	  bibtex-autokey-titleword-length 5)

      ;; (setq bibtex-completion-pdf-open-function
      ;; 	  (lambda (fpath)
      ;; 		(call-process "open" nil 0 nil fpath))))

      )
