;;   _____ __  __    _    ____ ____  
;;  | ____|  \/  |  / \  / ___/ ___| 
;;  |  _| | |\/| | / _ \| |   \___ \ 
;;  | |___| |  | |/ ___ \ |___ ___) |
;;  |_____|_|  |_/_/   \_\____|____/ 
;;                                   

;; NOTE: init.el is generated from EmacsTestConfig.org.
 ;; Please change your settings in that file
 ;; using Emacs and init.el will be generated automatically!

;; Setting Variables
;; for better customization and readability

;; Save Dotfiles Dirs for later use
(setq pet/dotfiles-dir
      (expand-file-name
       (convert-standard-filename
       "~/.dotfiles/")))
(setq pet/dotfiles-emacsconfig-dir
        (concat pet/dotfiles-dir
         "C01_EmacsConfiguration/"))

;; Adjust font size to match your system
(defvar pet/default-font-size 140)
(defvar pet/default-variable-font-size 120)

;; bootstrap script to install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Make sure to always install packages (pendant to use-package-always-ensure)
(setq straight-use-package-by-default t)

;; A few basic settings

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Start Emacs in Fullscreen mode
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Set default Encoding to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka" :height pet/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height pet/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height pet/default-font-size :weight 'regular)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; default for auto-save-list-file-prefix is "~/.emacs.d/auto-save-list/.saves~"
;; this moves it to a more centralized location (tmp)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Show Calendar on StartUp                      
(calendar)

;; set date format to %DD-%MM-%YYYY
(setq european-calender-style 't)

; Setup file containing global macros
(load-file
 (concat pet/dotfiles-emacsconfig-dir
         "macros/global.macs")) 

;; Set of keybindings for defined macros
;; Make sure to have a definition of the macro in your /macros folder
(global-set-key "\C-x\C-kT" 'transpose-names)

;; Set Location for bookmarks file/s
(setq bookmark-default-fil
      (concat pet/dotfiles-emacsconfig-dir
              "bookmarks"))

;; Activate Abbrev Mode by default
(setq-default abbrev-mode t)

;; Set Location and Name of Abbrev file
(setq abbrev-file-name
      (concat pet/dotfiles-emacsconfig-dir
              "abbrev_defs"))

;; Save Abbrevs when saving Files
(setq save-abbrevs t)

(setq pet/yasnippet-dir
      (concat pet/dotfiles-emacsconfig-dir
              "snippets"))

;; Yasnippets
(use-package yasnippet
  :config
  ;; Set Yasnippet dir
  (setq yas-snippet-dirs '(pet/yasnippet-dir))

  ;; Activate Yasnippets globally
  (yas-global-mode 1)

  ;; Enable snippets being shared between modes
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode
               'fundamental-mode))))

;; Multiple cusors are a must. Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "C-S-c C-S-c")
                  'mc/edit-lines)
  (global-set-key (kbd "C-<")
                  'mc/mark-previous-like-this)
  (global-set-key (kbd "C->")
                  'mc/mark-next-like-this)
  (global-set-key (kbd "C-c M-<")
                  'mc/mark-all-like-this)
  (global-set-key (kbd "s-D")
                  'mc/mark-all-dwim)
  (define-key mc/keymap (kbd
                         "<return>") nil))

;; Setup general for easier key config
(use-package general
  :config
  (general-create-definer pet/leader-keys
  :prefix "C-."
  :global-prefix "C-.")

  (pet/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme
          :which-key "choose theme")))

;; Enable Command Log Mode
(use-package command-log-mode)

;; Load Doom Themes
(use-package doom-themes
  ;; :init (load-theme 'doom-dracula t)
  )

;; Use all-the-icons
;;required for doom modeling
(use-package all-the-icons)

;; Load doom modeline
(use-package doom-modeline
  ;; Activate Doom Modeline
  ;; :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Load which-key
;; Loads a more helpful UI Completion buffer 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Tab Bar Mode Setting

;; Set new tab to scratch buffer
(setq tab-bar-new-tab-choice "*scratch*")
;; right is default -
;; change if you dont like that
;; (tab-bar-new-tab-to right)                

;; Set the name of the tab to
;; match the current buffer
;; (setq tab-bar-tab-name-function
;;       tab-bar-current-tab-name)

  ;; Keyboard Rules
  ;; Remove Tab Bar Buttons
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        ;; tab-bar-button-relief               ;; controls outline of buttons
        ;; tab-bar-face tab-bar-tab            ;; configure tab face (bgcolor etc.)
        )

  ;; tab bar is not automatically shown
  ;; (set 1 to enable)
  (setq tab-bar-show nil)                      

  ;; Helper function to get only the name
  ;; of current tab
  (defun pet/current-tab-name ()
    (alist-get 'name (tab-bar--current-tab)))

(use-package treemacs
  :bind
  (:map global-map
        ([f8] . treemacs)
        ("C-<f8>" . treemacs-select-window))
  :config
  ;; ensure that treemacs-buffer is
  ;; ignored when switching windows 
  (setq treemacs-is-never-other-window t)

  ;; Add shortcut for treemacs to
  ;; personal keyspace
  (pet/leader-keys
    "tt" 'treemacs
    "tw" 'treemacs-select-window)
  )

(use-package elfeed
  :bind (("C-c f" . elfeed)
         :map elfeed-search-mode-map
         ("n" . (lambda () (interactive) (next-line) (call-interactively 'elfeed-search-show-entry)))
         ("p" . (lambda () (interactive) (previous-line) (call-interactively 'elfeed-search-show-entry)))
         ("m" . (lambda () (interactive) (apply 'elfeed-search-toggle-all '(star))))
         ("g" . elfeed-update)
         ("G" . elfeed-search-update--force)
         ;;:map elfeed-show-mode-map
         ;;("w" . elfeed-show-yank))
     )
:config
(setq elfeed-show-entry-switch 'display-buffer)
(setq elfeed-search-remain-on-entry t)
 ;; Various Necessary/Helpful Settings
(setq elfeed-use-curl t)
(setq elfeed-curl-max-connections 10)
(setq elfeed-db-directory
      (concat pet/dotfiles-emacsconfig-dir
              "elfeed/"))
(setq elfeed-enclosure-default-dir
      "~/Downloads/")
(setq elfeed-search-filter
      "@4-months-ago +unread")
(setq elfeed-sort-order 'descending)
(setq elfeed-search-clipboard-type 'CLIPBOARD)
(setq elfeed-search-title-max-width 150)
(setq elfeed-search-title-min-width 30)
(setq elfeed-search-trailing-width 25)
(setq elfeed-show-truncate-long-urls t)
(setq elfeed-show-unique-buffers t)
(setq elfeed-search-date-format
      '("%F %R" 16 :left))

;; Snippet for periodic update for feeds
;; (3 mins since Emacs start, then every
;; half hour)
(run-at-time 180 1800
             (lambda ()
               (unless elfeed-waiting
                 (elfeed-update))))
)
;; Load Feeds and Feed Settings  
(load (concat pet/dotfiles-emacsconfig-dir
              "EmacsRSSFeed.el"))

;; Load Elfeed Score
(use-package elfeed-score
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "="
                elfeed-score-map))
  (setq elfeed-search-print-entry-function
        #'elfeed-score-print-entry)
  (setq elfeed-score-serde-score-file
        (concat pet/dotfiles-emacsconfig-dir
         "elfeed.score"))
  (setq elfeed-score-rule-stats-file
        (concat pet/dotfiles-emacsconfig-dir
                "elfeed.stats")))

(use-package org
  :bind (("C-c l" . org-store-link))
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory (convert-standard-filename "~/Org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; setup inline previewing of latex fragments
  (setq org-latex-create-formula-image-program 'imagemagick)

  (setq org-agenda-files
        '("~/Org/journal"
          "~/Org/personal-tasks.org"
          "~/Org/personal-mail.org"
          "~/Org/personal-chores.org"))

  )

;; Org Capture helper Function
(defun pet/create-documents-file ()
  "Create an org file in ~/Org/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name
     (format "%s.org" name))))

;; Org-Capture
(use-package org-capture
  :straight nil
  :config
   (setq org-capture-templates
         ;; Acronym captures
         `(("a" "Acronyms")

           ("ag" "General Acronyms")
           ("agg" "General Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("agt" "General Acronyms - Terminology" table-line
            (file+olp "~/Org/acronyms.org" "General"
                      "Terminology")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("as" "Scientific Acronyms")
           ("ase" "Scientific Acronyms - Economy" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Economy")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asg" "Scientific Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asm" "Scientific Acronyms - Maths" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Maths")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")
           ("asp" "Scientific Acronyms - Physics" table-line
            (file+olp "~/Org/acronyms.org" "Science"
                      "Physics")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

           ("ai" "IT related Acronyms")
           ("aic" "IT related Acronyms - Encryption" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encryption")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aim" "IT related Acronyms - Mail" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Mail")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aie" "IT related Acronyms - Emacs" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Emacs")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aig" "IT related Acronyms - General" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "General")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aii" "IT related Acronyms - Internet" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Internet")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ail" "IT related Acronyms - LaTeX" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "LaTeX")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("ain" "IT related Acronyms - Networks" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Networks")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aip" "IT related Acronyms - Programming" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Programming")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")
           ("aiu" "IT related Acronyms - Encoding" table-line
            (file+olp "~/Org/acronyms.org" "IT"
                      "Encoding")
            "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")  


           ;; Documents
           ("d" "Documents")
           ("dl" "Letter")
           ("dlf" "Letter Form" plain (file pet/create-documents-file)
            "%[~/.dotfiles/00_OrgFiles/Templates/Capture-LetterTemp.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )
           ("dlh" "Letter Home" plain (file pet/create-documents-file)
            "%[~/Templates/X1_Emacs_Templates/Capture-LetterTemp-Filled-Home-Real.org]"
            :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
            :unnarrowed t
            )


           ;; Email captures
           ("e" "Email")
           ("em" "Make email note" entry
            (file+headline "~/Org/personal-tasks.org" "Mail correspondence")
            ,(concat "* TODO [#A] %:subject :mail:\n"
                     "SCHEDULED: %t\n:"
                     "PROPERTIES:\n:CONTEXT: %a\n:END:\n\n"
                     "%i%?"))
           ("ef" "Follow Up" entry (file+olp "~/Org/personal-mail.org" "Follow Up")
            "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)
           ("er" "Read Later" entry (file+olp "~/Org/personal-mail.org" "Read Later")
            "* TODO Read %:subject %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i \n\n" :immediate-finish t)


           ;; Journal captures
           ("j" "Journal Entries")
           ("jj" "Journal" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
            ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
            :clock-in :clock-resume
            :empty-lines 1)
           ("jm" "Meeting" entry
            (file+olp+datetree "~/Org/journal/journal.org")
            "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
            :clock-in :clock-resume
            :empty-lines 1)


           ;; Checklist captures
           ("l" "Lists")

           ("ls" "Shopping List")
           ("lsp" "Permanent & Long Lasting")
           ("lspw" "Living" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =")
            "%^{Itemname}")
           ("lspd" "Technology" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Technik =")
            "%^{Itemname}")
           ("lspdc" "Computer" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Computer =")
            "%^{Itemname}")
           ("lspdh" "Appliances" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Wohnung =" "TODO = Haushaltsgeräte =")
            "%^{Itemname}")
           ("lspt" "Transport" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Permanentgüter =" "TODO = Transport =")
            "%^{Itemname}")
           ("lsv" "Consumables & Usables")
           ("lsvb" "Office Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Büromaterial =")
            "%^{Itemname}")
           ("lsvl" "Groceries" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Lebensmittel =")
            "%^{Itemname}")
           ("lsvr" "Cleaning Supplies" checkitem
            (file+olp "~/Org/lists-shopping.org" "TODO = Verbrauchsgüter =" "TODO = Reinigungs- und Pflegemittel =")
            "%^{Itemname}")

           ("ll" "Literature")
           ("lls" "Scientific Literature")
           ("llsb" "Biology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsc" "Chemistry" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Chemie ==") "[ ] %^{Author} - %^{Title}")
           ("llse" "Politics, Economy and Ecology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Politik, Ökonomie und Ökologie ==") "[ ] %^{Author} - %^{Title}")
           ("llsg" "History" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== History ==") "[ ] %^{Author} - %^{Title}")
           ("llsh" "Medicine and Health" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Medizin ==") "[ ] %^{Author} - %^{Title}")
           ("llsi" "IT" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Informatik, Data-Science und AI ==") "[ ] %^{Author} - %^{Title}")
           ("llsm" "Maths" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Mathematik ==") "[ ] %^{Author} - %^{Title}")
           ("llsp" "Physics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Physik ==") "[ ] %^{Author} - %^{Title}")
           ("llss" "Philosophy and Sociology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Philosophie und Soziologie ==") "[ ] %^{Author} - %^{Title}")
           ("llst" "Technology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Technik ==") "[ ] %^{Author} - %^{Title}")
           ("llsl" "Languages" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Sprachen ==") "[ ] %^{Author} - %^{Title}")
           ("llsz" "Psychology" checkitem
            (file+olp "~/Org/lists-literature.org" "= Sachbücher =" "== Psychologie ==") "[ ] %^{Author} - %^{Title}")

           ("llr" "Novels" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =") "[ ] %^{Author} - %^{Title}")
           ("llrk" "Classics" checkitem
            (file+olp "~/Org/lists-literature.org" "= Romane =" "== Klassiker ==") "[ ] %^{Author} - %^{Title}")


           ("lm" "Music")
           ("lmd" "Downlaodable" checkitem
            (file+olp "~/Org/lists-music.org" "TODO Musik zum Downloaden")
            "[ ] %^{Interpret} - %^{Title}")

           ("q" "Quotes")
           ("qt" "Talks" entry
            (file+olp "~/Org/personal-quotes.org" "Reden und Interviews")
            "* %^{Originator} \n %?")
           ("ql" "Literature" entry
            (file+olp "~/Org/personal-quotes.org" "Literatur")
            "* %^{Originator} \n %?")


           ("t" "Tasks / Projects")
           ("tt" "TODO Task" entry (file+olp "~/Org/personal-tasks.org" "Inbox")
            "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)  
           ("tb" "Basic task for future review" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Title}\n"
                     ":PROPERTIES:\n"
                     ":CAPTURED: %U\n"
                     ":END:\n\n"
                     "%i%l"))
           ("ts" "Task with a due date (scheduled)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "SCHEDULED: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))
           ("td" "Task with a due date (deadline)" entry
            (file+headline "~/Org/personal-tasks.org" "Inbox")
            ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                     "DEADLINE: %^t\n"
                     ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                     "%i%?"))


           ("w" "Workflows")
           ("we" "Checking Email" entry (file+olp+datetree "~/Org/journal/Journal.org")
            "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))


  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")
                (in-mode . "mu4e-headers-mode")))))
  :bind
  ("C-c c" . org-capture))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
                \\usepackage{hyperref}
                \\usepackage{babel}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
           '("org-plain-scrlttr2-german"
             "\\documentclass[a4paper, 
              parskip=half,%
              fromalign=right, 
              fromrule=false, 
              11pt, ngerman]{scrlttr2}
              \\usepackage{hyperref}
              \\usepackage{babel}
         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Bigger LaTeX Previews
(plist-put org-format-latex-options :scale 1.5)
;; Load language packages for pdflatex of lualatex / xelatex compilers
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "babel" t ("pdflatex")))
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Org")
  (org-roam-dailies-directory "journal/")

  (org-roam-completion-everywhere t)

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
      ;; org roam capture templates
  (setq org-roam-capture-templates
        `(("d" "default" plain
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
          :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t)  
          ("b" "book notes" plain (file "~/.dotfiles/00_OrgFiles/Templates/RoamCapture-BookNoteTemp.org")
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n#+filetags: Project")
           :unnarrowed t)
          ))


  ;; dailies capture template
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n"))))

  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-drill
  :config
  (progn
    (add-to-list 'org-modules 'org-drill)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-hint-separator "||")
    (setq org-drill-left-cloze-delimiter "<[")
    (setq org-drill-right-cloze-delimiter "]>")
    (setq org-drill-learn-fraction 1.0))
  )

;; Setup Automatic Tangling of Files

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-config ()
  (when (string-equal
         (buffer-file-name)
         (concat pet/dotfiles-dir
                 "000_OrgFiles/EmacsTestConfig.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             #'pet/org-babel-tangle-config)))

;; Automatically tangle test config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-testconfig ()
  (when (string-equal
         (buffer-file-name)
         (concat pet/dotfiles-dir
                 "000_OrgFiles/EmacsTestConfig.org"))
      (org-babel-tangle)))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             #'pet/org-babel-tangle-testconfig)))

;; Setup Automatic Tangling of Files

;; Automatically tangle config file
;; Helper Function to that does the tangling
(defun pet/org-babel-tangle-feeds ()
  (when (string-equal
         (buffer-file-name)
         (concat pet/dotfiles-dir
                 "000_OrgFiles/EmacsRSSFeed.org"))

    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; This hook automatically evaluates the helper
;; function after saving the buffer
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook
             'after-save-hook
             #'pet/org-babel-tangle-feeds)))
