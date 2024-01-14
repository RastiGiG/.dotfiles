;; Helper Functions for Org
(defun pet/org-font-setup ()
      ;; Set faces for heading levels
      (dolist (face '((org-level-1 . 1.2)
		      (org-level-2 . 1.15)
		      (org-level-3 . 1.1)
		      (org-level-4 . 1.05)
		      (org-level-5 . 1.02)
		      (org-level-6 . 1.0)
		      (org-level-7 . 1.0)
		      (org-level-8 . 1.0)))
	(set-face-attribute
	 (car face)
	 nil
	 :font "Cantarell"
	 :weight 'regular
	 :height (cdr face)))

      ;; Ensure that anything that should be
      ;; fixed-pitch in Org files appears that way
      (set-face-attribute 'org-block nil
		      :foreground nil
		      :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil
		      :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-table nil
		      :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil
		      :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil
		      :inherit '(font-lock-comment-face
				 fixed-pitch))
      (set-face-attribute 'org-meta-line nil
		      :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil
		      :inherit 'fixed-pitch))

;; Replace list hyphen with dot
(defun pet/org-replace-hyphen ()
      (font-lock-add-keywords
       'org-mode '(("^ *\\([-]\\) "
		(0 (prog1 () (compose-region
			      (match-beginning 1)
			      (match-end 1) "•"))))))
      )

;; Helper Function to quickly toggle Babel Confirm Evaluation
(defun pet/org-toggle-babel-confirm-evaluate ()
(interactive)
"Toogle org-babel-confirm-evaluate on/ff"
(if org-confirm-babel-evaluate
	(setq org-confirm-babel-evaluate nil)
      (setq org-confirm-babel-evaluate t))
(print (concat "Org Babel Confirm State: "
	       (format "%s" org-confirm-babel-evaluate))))

;; Add Org Contrib Packages
(use-package org-contrib)

;; Setting Up Org Mode
(use-package org
:ensure org-plus-contrib
:straight nil
:bind (("C-c l" . org-store-link))
:hook (org-mode . org-indent-mode)
:custom
(org-cite-export-processors
 '((md . (csl "chicago-fullnote-bibliography.csl"))   ; Footnote reliant
   (latex . biblatex)                                 ; For humanities
   (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
   (t . (csl "modern-language-association.csl"))      ; Fallback
   ))
;; Add Citation Directory
(org-cite-csl-styles-dir
 (concat pet/temp-dir "X6_Citation_Styles/"))
:custom-face
;; Have citation link faces look closer to as they were for `org-ref'
(org-cite ((t (:foreground "DarkSeaGreen4"))))
(org-cite-key ((t (:foreground "forest green" :slant italic))))
)

;; Add additional Export Options
(require 'ox-beamer)       ;; LaTeX beamer
(require 'ox-koma-letter)  ;; LaTeX KOMA Script
(require 'ox-md)           ;; Markdown
(require 'ox-texinfo)      ;; Texinfo
(require 'ox-man)          ;; Man Page
(require 'ox-org)          ;; Org Format

;; Add additional Babel Support
(require 'ob-ledger)       ;; Ledger

;; Add additional Citation Support
(require 'oc-basic)        ;; Basic
(require 'oc-bibtex)       ;; Bibtex
(require 'oc-biblatex)     ;; Biblatex
(require 'oc-csl)          ;; CSL

(setq org-ellipsis " ▾")

;; Default directory for Org Files for Captures and Agenda
(setq org-directory pet/org-dir)

;; Set Org Clock Sound File
(setq org-clock-sound (concat pet/org-dir "sounds/Erfolg.wav"))
;; (setq org-clock-sound t)

;; Startup with inline images displayed
(setq org-startup-with-inline-images t)

;; Enable helper function replacing hyphen
(pet/org-replace-hyphen)

;; The default here is 999, which is a little to constricting for SQL and such
(setq org-table-convert-region-max-lines 9999)

;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda (c)
                           (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; ;; Set src block automatic indent to 0 instead of 2.
;; (setq org-edit-src-content-indentation 0)

;; Specify Agenda Files
(setq org-agenda-files
  ;;(cons (concat pet/org-dir "journal")
      ;; Add Files a starting with "personal-"
      (directory-files pet/org-dir t
  		       "personal-\\(tasks\\|mail\\|chores\\|contracts\\)-?[A-Za-z]*.org")
      ;;)
)

(setq org-agenda-start-with-log-mode t)

;; Use Drawer to store Information aboutTodo State Changes
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Add Custom TODO Keywords - in 2 seperate Sequences
(setq org-todo-keywords
      ;; Sequence 1 
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    	  ;; Sequence 2
    	  (sequence "MEET(m)")
    	  ;; Sequence 3
    	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)"
    		        "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)"
    		        "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; Customize Apps for Filelinks
(cl-loop for type in
  	       '(;; Open PDFs with Zathura
  		 ;; ("\\.pdf\\'" . "zathura %s") ;
  		 ;; Open PDFs with PDF Tools
  		 ("\\.pdf\\'" . "pdf-tools %s")			   
  		 ;; Open Pictures with sxiv 
  		 ("\\.png\\'" . "sxiv %s")
  		 ("\\.jpg\\'" . "sxiv %s")
  		 ("\\.jpeg\\'" . "sxiv %s")
  		 ("\\.svg\\'" . "sxiv %s")
  		 ;; Open Youtube links with freetube
  		 ("\\.\\*youtu\\.\\*" . "freetube %s"))
  	       do
  	       (add-to-list 'org-file-apps type))

;; Set Keywords with shortcuts
(setq org-tag-alist
  '((:startgroup)
      ;; Put mutually exclusive tags here
      (:endgroup)
      ("@errand" . ?E)
      ("@home" . ?H)
      ("@work" . ?W)
      ("@study" . ?S)
      ("agenda" . ?a)
      ("planning" . ?p)
      ("publish" . ?P)
      ("batch" . ?b)
      ("note" . ?n)
      ("idea" . ?i)))

;; Set Refile Targets to be considered, Emphasis on Archive 
(setq org-refile-targets
  '((nil :maxlevel . 9)
      ("~/Org/personal-archive.org" :maxlevel . 1)
      ("~/Org/personal-tasks.org" :maxlevel . 1)
      ("~/Org/personal-sources.org" :maxlevel . 1)
      ("~/Backup/Web-Bookmarks/1-bookmarks-import.org" :maxlevel . 9)
      ("~/Backup/Web-Bookmarks/2-bookmarks-export.org" :maxlevel . 9)))

;; Allow Creation of Parent nodes but ask for confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(pet/leader-keys
  "ot" '(:ignore t :which-key "Toggle")
  "otb" '(pet/org-toggle-babel-confirm-evaluate
  	:which-key "Babel Confirm Evaluation")
  "otc" '(org-cdlatex-mode
  	:which-key "Org CDLaTeX Minor Mode")
  "oti" '(org-toggle-inline-images
  	:which-key "Inline Images")
  "otp" '(org-toggle-pretty-entities
  	:which-key "Pretty entities")
  "oi" '(:ignore t :which-key "Import")
  "oit" '(org-table-import
  	:which-key "Table")
  "oa"  '(org-agenda :which-key "Org Agenda"))

;; Setup inline previewing of latex fragments
(setq org-latex-create-formula-image-program
  'imagemagick)

;; Set Latex PDF Export Process
(setq org-latex-pdf-process
  	(list
  	 "latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"
  	 "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))

;; Add global Bibliography Source
(setq org-cite-global-bibliography
  	(list
  	 pet/main-bib))

;; Bigger LaTeX Previews
(plist-put org-format-latex-options :scale 1.5)

;; Load language packages for pdflatex of lualatex / xelatex compilers
(add-to-list 'org-latex-packages-alist
    		       ;; Language Support
    		       '("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Language Support
    		       '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; For Modern Fonts, Vektorschrift
    		       '("" "lmodern" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Tabellen mit variabler Breite
    		       '("" "tabularx" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Source Code Lists
    		       '("" "listings" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Support for more file names for graphics package
    		       '("" "grffile" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Mathematical Enhancer
    		       '("" "amsmath" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Mathematical Enhancer
    		       '("" "amsthm" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Mathematical Enhancer
    		       '("" "amssymb" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Allows adjusting of counter in enumerate environment
    		       '("" "enumerate" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; For Modern Fonts, Vektorschrift
    		       '("" "lmodern" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Fix typessetting in amsmath, extend amsmath 
    		       '("fixamsmath, dissallowspace" "mathtools" t
    			 ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; More Symbols
    		       '("" "marvosym" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Even More Math Symbols
    		       '("" "esint" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Color Support and Adjustment functionality
    		       '("" "color" t ("pdflatex" "xelatex" "lualatex")))
(add-to-list 'org-latex-packages-alist
    		       ;; Adds more color variations, more options for color specification
    		       '("svgnames, dvipsnames" "xcolor" t ("pdflatex" "xelatex" "lualatex")))

;; Org LaTeX Setup
(eval-after-load 'ox-latex
'(progn
   ;; Plain Article Class
   (add-to-list 'org-latex-classes
  			      '("plain-article"
  				"\\documentclass{article}
  		      [NO-DEFAULT-PACKAGES]
  		      [PACKAGES]
  		      [EXTRA]"
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   ;; CV Class
   (add-to-list 'org-latex-classes
  			      '("moderncv"
  				"\\documentclass[11pt,
  				a4paper,
  				sans, 
  				]{moderncv}
  		 [NO-DEFAULT-PACKAGES]
  		 [PACKAGES]
  		 [EXTRA]"
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;; Org LaTeX Setup
(eval-after-load 'ox-latex
'(progn
   ;; Add Koma Article
   (add-to-list 'org-latex-classes
  			      '("scrartcl"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrartcl}
  	 [DEFAULT-PACKAGES]
  	 [PACKAGES]
  	 [EXTRA]"
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
   ;; Alternative Koma Article Name
   (add-to-list 'org-latex-classes
  			      '("koma-article"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrartcl}
  	 [DEFAULT-PACKAGES]
  	 [NO-PACKAGES]
  	 [EXTRA]"
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   ;; Add Koma Report
   (add-to-list 'org-latex-classes
  			      '("scrreprt"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrreprt}
  	 [DEFAULT-PACKAGES]
  	 [PACKAGES]
  	 [EXTRA]"
  				("\\part{%s}" . "\\part*{%s}")
  				("\\chapter{%s}" . "\\chapter*{%s}")
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
   ;; Alternative Koma Report name
   (add-to-list 'org-latex-classes
  			      '("koma-report"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrreprt}
  	 [DEFAULT-PACKAGES]
  	 [NO-PACKAGES]
  	 [EXTRA]"
  				("\\part{%s}" . "\\part*{%s}")
  				("\\chapter{%s}" . "\\chapter*{%s}")
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
   ;; Alternative Koma Report Structure
   (add-to-list 'org-latex-classes
  			      '("koma-report-shortened"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrreprt}
  	 [DEFAULT-PACKAGES]
  	 [NO-PACKAGES]
  	 [EXTRA]"
  				("\\chapter{%s}" . "\\chapter*{%s}")
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


   ;; Add Koma Book
   (add-to-list 'org-latex-classes
  			      '("scrbook"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrbook}
  	 [DEFAULT-PACKAGES]
  	 [PACKAGES]
  	 [EXTRA]"
  				("\\part{%s}" . "\\part*{%s}")
  				("\\chapter{%s}" . "\\chapter*{%s}")
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
   ;; Alternative Koma Book name
   (add-to-list 'org-latex-classes
  			      '("koma-book"
  				"\\documentclass[a4paper, 
  		      parskip=half,%
  		      fromalign=right, 
  		      fromrule=false, 
  		      11pt
  		      ]{scrbook}
  	 [DEFAULT-PACKAGES]
  	 [NO-PACKAGES]
  	 [EXTRA]"
  				("\\part{%s}" . "\\part*{%s}")
  				("\\chapter{%s}" . "\\chapter*{%s}")
  				("\\section{%s}" . "\\section*{%s}")
  				("\\subsection{%s}" . "\\subsection*{%s}")
  				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  				("\\paragraph{%s}" . "\\paragraph*{%s}")
  				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;; Koma Letter Export Classes
(eval-after-load 'ox-koma-letter
  '(progn
     (add-to-list 'org-latex-classes
    			      '("scrlttr2"
    				"\\documentclass\{scrlttr2\}
   \[DEFAULT-PACKAGES]
   \[PACKAGES]
   \[EXTRA]"))
     (add-to-list 'org-latex-classes
    			      '("scrlttr2-german"
    				"\\documentclass[a4paper, 
    		      parskip=half,%
    		      fromalign=right, 
    		      fromrule=false, 
    		      11pt, 
    		      ngerman]{scrlttr2}
    	 [NO-DEFAULT-PACKAGES]
    	 [PACKAGES]
    	 [EXTRA]"
    				("\\section{%s}" . "\\section*{%s}")
    				("\\subsection{%s}" . "\\subsection*{%s}")
    				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    				("\\paragraph{%s}" . "\\paragraph*{%s}")
    				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
     (add-to-list 'org-latex-classes
    			      '("koma-letter"
    				"\\documentclass[a4paper,
    		      parskip=half,
    		      fromalign=right,
    		      fromrule=true,
    		      11pt,
    		      ngerman
    		      ]{scrlttr2}
    	 [NO-DEFAULT-PACKAGES]
    	 [PACKAGES]
    	 [EXTRA]"
    				("\\section{%s}" . "\\section*{%s}")
    				("\\subsection{%s}" . "\\subsection*{%s}")
    				("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    				("\\paragraph{%s}" . "\\paragraph*{%s}")
    				("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

     ;; Set Default Letter Class
     (setq org-koma-letter-default-class "scrlttr2")
     ;; Use Backaddress by default
     (setq org-koma-letter-use-backaddress t)))

;; Load Rust Support for Org Mode
(use-package ob-rust)

;; Load Nix Support for Org Mode
(use-package ob-nix)

;; (require 'ob-ledger)
;; (require-package 'ob-ipython)

;; enable/disable languages for org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)    ;; Elisp
    (lisp . t)          ;; Lisp
    (clojure . t)       ;; Clojure     
    (scheme . t)        ;; Scheme
    (python . t)        ;; Python
    ;; (ipython . t)       ;; IPython

    ;;  the following two require ob-c
    (C . t)             ;; C 

    (perl . t)          ;; Perl
    ;; (php . t)           ;; PHP
    (R . t)             ;; R
    ;; (Ruby . t)          ;; Ruby
    (rust . t)          ;; Rust
    (julia . t)         ;; Julia Programmin Language
    (lua . t)           ;; Lua Programming Language
    (shell . t)         ;; Command Line Programs 
    (latex . t)         ;; LaTeX  
    (nix . t)           ;; Nix
    (sql . t)           ;; SQL
    (sqlite . t)        ;; SQLite
    (octave . t)        ;; Octave
    (makefile . t)          ;; GNU Make
    (gnuplot . t)       ;; Gnuplot
    (awk . t)           ;; awk
    (sed . t)           ;; GNUsed
    (css . t)           ;; CSS
    (plantuml . t)      ;; PlantUML
    (ledger . t)        ;; Ledger CLI
    ))

;; Add conf-unix to be recognized
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Load Org-Tempo Module (needed as of Org 9.2)
(require 'org-tempo)

;; Setup Source Block Templates
(cl-loop for block in
		 '(;; AWK
		       ("aw"   . "src awk")
		       ;; C and Cpp
		       ("cs"   . "src C")
		       ("cp"   . "src C++")
		       ;; Emacs-Lisp
		       ("el"   . "src emacs-lisp")
		       ;; JSON
		       ("json" . "src json")
		       ;; Lua
		       ("lua"  . "src lua")
		       ;; Ledger
		       ("ldg"  . "src ledger :noweb yes")
		       ;; LaTeX
		       ("ltx"   . "src latex")
		       ;; Nix
		       ("nix"   . "src nix")
		       ;; Makefile
		       ("mf"   . "src makefile")
		       ;; Octave
		       ("oc"   . "src octave")
		       ;; Perl
		       ("perl" . "src perl")
		       ;; PHP
		       ("ph"   . "src php")
		       ;; Python
		       ("py"   . "src python")
		       ;; Scheme
		       ("sc"   . "src scheme")
		       ;; Shell
		       ("sh"   . "src shell")
		       ;; SQL
		       ("sql"   . "src sql")
		       ;; YAML
		       ("yaml" . "src yaml")
		       ;; R
		       ("rp"   . "src R")                                  ;; pure R             
		       ("rr"   . "src R :results both output")             ;; R with output
		       ("rs"   . "src R :session :results both output")    ;; R with output
		       ;; Rust
		       ("ru"   . "src rust ")    ;; pure rust

		       ;; ("go" . "src go")
		       ;; ("ip" . "src ipython :session :async :exports both :results raw drawer")
		       ;; ("ts" . "src typescript"))
		       ;; This is an alternative Block
		       ;; For IPython
		       ;; ("si" . "src ipython :session :async :results output")
		       )
		 do
		 (add-to-list
		      'org-structure-template-alist block))

;; Org-Capture
(use-package org-capture
  :straight nil
  :init

  ;; Org Capture helper Variables

  ;; Save default Org Capture Template Dir
  (setq pet/org-cap-temp-dir
        (concat pet/temp-dir
                (convert-standard-filename
                 "X1_Emacs_Templates/Org_Capture_Templates/"
                 )))

  ;; Org Capture helper Function

  ;; Get filename and store at current directory
  (defun pet/get-file-name-without-extension ()
    "Ask user for filename"
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name name))
    )
  ;; Get filename with ".org" and store at current directory
  (defun pet/get-org-file-name ()
    "Ask user for filename"
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name
       (format "%s.org" name)))
    )

  :config
  ;; Set default file for capture if not specified
  (setq org-default-notes-file
        (concat pet/home-dir "Projects/Notes/notes.org"))

  (setq org-capture-templates
        ;; Acronym captures
        `(("a" "Acronyms" table-line
           (file+headline "~/Org/acronyms.org" "Inbox")
           "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|")

          ;; Documents
          ("d" "Documents")
          ("dl" "Letter")
          ("dlf" "Letter Form" plain (file pet/get-org-file-name)
           "%[~/.dotfiles/00_OrgFiles/Templates/Capture-LetterTemp.org]"
           :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t
           )
          ("dlh" "Letter Home" plain (file pet/get-org-file-name)
           "%[~/Templates/X1_Emacs_Templates/Capture-LetterTemp-Filled-Home-Real.org]"
           :if-new (file "${slug}.org" "#+TITLE: ${title}\n")
           :unnarrowed t
           )
          ("do" "Org File")
          ("dod" "Default Org File"
           plain
           (file pet/get-org-writing-file-name)
           "#+TITLE: ${title}\n%[~/Templates/X1_Emacs_Templates/Org_Capture_Templates/default-org-file.orgctemp]"
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

          ("ls" "Shopping List" checkitem
           (file+olp "~/Org/lists-shopping.org" "Inbox")
           "[ ] %^{Itemname}")

          ("ll" "Literature" checkitem
           (file+olp "~/Org/lists-literature.org" "Inbox")
           "[ ] %^{Author} - %^{Titel}")

          ("lm" "Music" checkitem
           (file+olp "~/Org/lists-music.org" "Inbox")
           "[ ] %^{Interpret} - %^{Title}")

          ("q" "Quotes" entry
           (file+olp "~/Org/quotes.org" "Inbox")
           "* %^{Originator}\n\n#+begin_quote\n%?\n#+end_quote")

          ("t" "Tasks / Projects")
          ("tt" "TODO Task" entry (file+olp
                                   "~/Org/personal-tasks.org" "Inbox")
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

  ;; Activate Context Templates for Email 
  (setq org-capture-templates-contexts
    '(("e" ((in-mode . "notmuch-search-mode")
            (in-mode . "notmuch-show-mode")
            (in-mode . "notmuch-tree-mode")
            (in-mode . "mu4e-headers-mode")))))
  :bind
  ("C-c c" . org-capture))

;; Setup Org Bullets
(use-package org-bullets
      :after org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Automatically create ToCs
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Org Roam is very handy to create a 'second brain'
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory pet/org-dir)
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
      ("w" "wiki")
      ("wn" "wiki node" plain
       "\n* ${title}\n\n%?" 
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
              "\n#+filetags: :%^{filetag}:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
       :unnarrowed t)
      ("wi" "wiki index node" plain
       "\n* ${title} Kompendium Index\n\n%?" 
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                  "\n#+filetags: :index:%^{filetag}:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
       :unnarrowed t)
      ("wr" "wiki references node" plain
       "\n* References\n%?\n** Websites\n\n** Literature" 
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                  "\n#+filetags: :%^{filetag}:references:\n#+TITLE: ${title}\n#+AUTHOR: %^{author}\n#+DATE: %U\n\n")
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
  ;; Ensure the keymap is available
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  (pet/leader-keys
    "or"  '(:ignore t :which-key "Org Roam")
    "ort" 'org-roam-tag-add
    "ora" 'org-roam-alias-add
    "ord" 'org-roam-diagnostics
    "oru" 'org-roam-ui-open
    )
  )

;; Helper Function to insert org note immediately
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
    (org-roam-capture-templates
     (list (append (car org-roam-capture-templates)
    	       '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; A Visualization of your org roam node structure
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui"
     :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
 :config
 (setq org-roam-ui-sync-theme t
  org-roam-ui-follow t
  org-roam-ui-update-on-save t
  org-roam-ui-open-on-start t))

;; Load Org Ref
(use-package org-ref
      :config
      ;; Bibtex Hydra
      ;; (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
      )

;; Add Org Drill to use Org as a Anki Backup
(use-package org-drill
      :config
      (progn
	(add-to-list 'org-modules 'org-drill)
	(setq org-drill-add-random-noise-to-intervals-p t)
	(setq org-drill-hint-separator "||")
	(setq org-drill-left-cloze-delimiter "<[")
	(setq org-drill-right-cloze-delimiter "]>")
	(setq org-drill-learn-fraction 1.0)))

;; Org AddOn Auto Tangle Org Files
;; Add '#+auto_tangle: t' to files 
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-crypt-key nil)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.

;; (setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;; Make org look a lot nicer
(use-package org-modern
  :defer
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))
