;; __     ___    ____  ___    _    ____  _     _____ ____  
;; \ \   / / \  |  _ \|_ _|  / \  | __ )| |   | ____/ ___| 
;;  \ \ / / _ \ | |_) || |  / _ \ |  _ \| |   |  _| \___ \ 
;;   \ V / ___ \|  _ < | | / ___ \| |_) | |___| |___ ___) |
;;    \_/_/   \_\_| \_\___/_/   \_\____/|_____|_____|____/ 
;;

;; Setting Variables
;; for better customization and readability

;; Save Home Dir for later use
(setq pet/home-dir
      (convert-standard-filename
       (expand-file-name "~/")))

;; Save Dotfiles Dir for later use
(setq pet/dotfiles-dir
      (concat pet/home-dir
      		      (convert-standard-filename
      		       ".dotfiles/")))

;; Save Template Dir for later use
(setq pet/temp-dir
      (concat pet/home-dir
      		      (convert-standard-filename
      		       "Templates/")))

;; Store Org Directory
(setq pet/org-dir
      (concat pet/home-dir
      		      (convert-standard-filename
      		       "Org/")))

;; Save Emacs Template Dir for later use
(setq pet/latex-header-temp-dir
      (concat pet/temp-dir
      		      (convert-standard-filename
      		       "X2_LaTeX_Templates/00-Headers/")))

;; Save Path to main Bibliography File
(setq pet/bibliography-dir
      (concat pet/home-dir
      		      "Projects/Writing/00_Bibliographies"))
;; Set main bibliography
(setq pet/main-bib
      (concat pet/bibliography-dir "/Main_Bib.bib"))

;; Save path to Emacs Configuration
(setq pet/dotfiles-emacsconfig-dir
      (concat pet/dotfiles-dir
      		      (convert-standard-filename
      		       "C01_EmacsConfiguration/")))

;; Set Path for Treesitter Language Grammars
(setq treesit-extra-load-path
    (list 
     (concat pet/dotfiles-dir
  		     "C00_GeneralEditorConfiguration/tree-sitter")))

;; Adjust font size to match your system
(defvar pet/default-font-size 140)
(defvar pet/default-variable-font-size 120)

;; Activate Abbrev Mode by default
(setq-default abbrev-mode t)

;; Set Location and Name of Abbrev file
(setq abbrev-file-name
      (concat pet/dotfiles-emacsconfig-dir
              "abbrev_defs"))

;; Save Abbrevs when saving Files
(setq save-abbrevs t)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; default for auto-save-list-file-prefix is "~/.emacs.d/auto-save-list/.saves~"
;; this moves it to a more centralized location (tmp)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Set Location for bookmarks file/s
(setq bookmark-default-file
      (concat pet/dotfiles-emacsconfig-dir
              "bookmarks"))

;; Store Backups in a single directory
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

; Setup file containing global macros
(pet/load-file
 (concat pet/dotfiles-emacsconfig-dir "macros/global.macs"))

;; Article Skeleton
(define-skeleton pet/latex-article-skeleton
  "Skeleton for article type latex documents"
  "Preamble:"
  "\\documentclass{article}\n"
  "\\usepackage[utf8]{inputenc}\n"
  "\\usepackage[margin=1 in]{geometry}\n"
  "\\usepackage{graphicx}\n"
  "\\setlength{\\parindent}{4em}\n"
  "\\setlength{\\parskip}{1em}\n"
  "\\renewcommand{\\baselinestretch}{1.5}\n\n"
  "\\author{<AUTOR>}\n"
  "\\title{"_"}\n"
  "\\date{\\today}\n\n"
  "\\begin{document}\n"
  "\\maketitle\n\n"
  "\\end{document}\n")

;; Org LaTeX Summary Header 
(define-skeleton pet/org-latex-summary-skeleton
  "Skeleton for summaries "
  "Preamble:"
  "#+LATEX_CLASS: article\n"
  "#+LATEX_CLASS_OPTIONS: [a5paper,landscape,fourcolumn]\n"
  "#+LATEX_COMPILER: lualatex\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "summaryheader.tex}\n"))
  "#+STARTUP: showeverything\n"
  "#+OPTIONS: toc:nil\n"
  "\\begin{multicols*}{4}\n"
  "* "_"\n"
  "\\end{multicols*}\n")

;; Org LaTeX Article Header
(define-skeleton pet/org-latex-article-skeleton
  "Skeleton for articles "
  "Preamble:"
  "#+STARTUP: showeverything\n"
  "#+TITLE: TITLE\n"
  "#+AUTHOR: AUTHOR\n"
  "#+DATE: \\today\n"
  "#+LATEX_CLASS: article\n"
  "#+LATEX_CLASS_OPTIONS: [a4paper]\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "articleheader.tex}\n"))
  "#+OPTIONS: toc:nil\n")

;; Org LaTeX Beamer Header
(define-skeleton pet/org-latex-beamer-skeleton
  "Skeleton for Beamer Presentations"
  "Preamble:"
  "#+STARTUP: beamer\n"
  "#+TITLE: TITLE\n"
  "#+AUTHOR: AUTHOR\n"
  "#+DATE: \\today\n"
  "#+LaTeX_CLASS: beamer\n"
  "#+LaTeX_CLASS_OPTIONS: [final]\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "beamerheader.tex}\n"))
  "#+STARTUP: showeverything\n"
  "#+OPTIONS: toc:nil\n")

;; Org LaTeX ModernCV Header
(define-skeleton pet/org-latex-moderncv-skeleton
  "Skeleton for CVs "
  "Preamble:"
  "#+LaTeX_CLASS: moderncv\n"
  "#+LaTeX_CLASS_OPTIONS: [11pt, a4paper, sans]\n"
  (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "moderncvheader.tex}\n"))
  "#+STARTUP: showeverything\n"
  "#+OPTIONS: toc:nil\n")

;; Org LaTeX Letter Header
(define-skeleton pet/org-latex-koma-letter-skeleton
      "Skeleton for Letters using KOMA-Script"
      "Preamble:"
      "#+LaTeX_CLASS: scrlttr2\n"
      "#+LaTeX_CLASS_OPTIONS: [11pt, a4paper, parskip=yes]\n"
      (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "letterheaderdefault.tex}\n"))
      (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "letterinfobasic.tex}\n"))
      "#+STARTUP: showeverything\n"
      "#+OPTIONS: toc:nil"
      "#+OPTIONS: num:nil"
      "#+OPTIONS: author:nil"
      "#+OPTIONS: title:nil"
      )

;; Org LaTeX Letter Header German
(define-skeleton pet/org-latex-koma-letter-german-skeleton
      "Skeleton for Letters using KOMA-Script - German Version"
      "Preamble:"
      "#+LaTeX_CLASS: scrlttr2-german\n"
      "#+LaTeX_CLASS_OPTIONS: [11pt, a4paper, parskip=yes]\n"
      (concat "#+LATEX_HEADER: \\input{" (concat pet/latex-header-temp-dir "letterheaderdefault.tex}\n"))
      "#+STARTUP: showeverything\n"
      "#+OPTIONS: toc:nil"
      "#+OPTIONS: num:nil"
      "#+OPTIONS: ':t backaddress:t"
      )

;; Rebind 'M-x' to 'C-C C-m'
(global-set-key (kbd "C-C C-m") 'execute-extended-command)

;; Set of keybindings for defined macros
;; Make sure to have a definition of the macro in your /macros folder
(global-set-key "\C-x\C-kT" 'transpose-names)
