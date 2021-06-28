;; NOTE: init.el is now generated from EmacsConfig.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 160)
(defvar efs/default-variable-font-size 160)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Start Emacs in Fullscreen mode
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

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

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-font-size :weight 'regular)

(calendar)                  ; Makes Emacs show the calendar on startup

;; set date format to %DD-%MM-%YYYY
(setq european-calender-style 't)

(load-file "~/.dotfiles/C1_EmacsConfiguration/macros/global.macs") ; Save file containing global macros

;; Set of keybindings for defined macros
;; Make sure to have a definition of the macro in your /macros folder
(global-set-key "\C-x\C-kT" 'transpose-names)

(setq bookmark-default-file "~/.dotfiles/C1_EmacsConfiguration/bookmarks")

(setq-default abbrev-mode t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
     "~/.dotfiles/C1_EmacsConfiguration/abbrev_defs") 
(setq save-abbrevs t)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.dotfiles/C1_EmacsConfiguration/snippets"))
  (yas-global-mode 1))

(use-package general
:config
(general-create-definer rune/leader-keys
  :prefix "C-."
  :global-prefix "C-. m")

(rune/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil
                             :timeout 10)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(defhydra hydra-bookmark-menu (:color pink
                               :hint nil
                               :timeout 10)
  "
       ^Mark^                ^Actions^              ^Search^            ^Annotations
      ^^^^^^^^-----------------------------------------------------------------------
      _m_: mark         _x_: execute          _/_: isearch             _a_: show
      _u_: unmark       _r_: rename           _w_: locate              _A_: show all
      _U_: unmark up    _R_: relocate bmk     _S_: show filenames      _e_: edit
      _d_: delete       _s_: save bmk list    _T_: hide filenames
      _D_: delete up    _l_: load bmk list    _t_: toggle filenames
      "
  ("m" bookmark-bmenu-mark)
  ("u" bookmark-bmenu-unmark)
  ("U" bookmark-bmenu-backup-unmark)
  ("d" bookmark-bmenu-delete)
  ("D" bookmark-bmenu-delete-backwards)
  ("x" bookmark-bmenu-execute-deletions)
  ("r" bookmark-bmenu-rename)
  ("R" bookmark-bmenu-relocate)  
  ("s" bookmark-bmenu-save)
  ("l" bookmark-bmenu-load)  
  ("/" bookmark-bmenu-search)
  ("w" bookmark-bmenu-locate)
  ("S" bookmark-bmenu-show-filenames)  
  ("T" bookmark-bmenu-hide-filenames)
  ("t" bookmark-bmenu-toggle-filenames)
  ("a" bookmark-bmenu-show-annotation)
  ("A" bookmark-bmenu-show-all-annotations)
  ("e" bookmark-bmenu-edit-annotation)
  ("c" nil "cancel")
  ("v" bookmark-bmenu-select "select" :color blue)
  ("o" bookmark-bmenu-other-window "other-window" :color blue)
  ("C-o" bookmark-bmenu-switch-window "switch-other-window" :color blue)
  ("1" bookmark-bmenu-1-window "full-frame" :color blue)
  ("2" bookmark-bmenu-2-window "show-on-split-window" :color blue)
  ("5" bookmark-bmenu-other-frame "other-frame" :color blue)
  ("q" quit-window "quit" :color blue))

;;  (define-key bookmark-bmenu-mode-map "." 'hydra-bookmark-menu/body)

(rune/leader-keys
  "b"  '(:ignore lm :which-key "bookmarks")
  "bl" '(bookmark-bmenu-list :which-key "bookmark list")
  "bm" '(hydra-bookmark-menu/body :which-key "bookmark list menu"))

(defhydra hydra-global-org-menu (:color blue
                                   :timeout 4)
  " Org Utilities

    ^Timer^                ^Clock^              ^Capture
    ^^^^^^-------------------------------------------------------------------------
     _t_: Start         _w_: Clock-In          _c_: Capture
     _s_: Stop          _o_: Clock-Out         _l_: Last Capture
     _r_: Set           _j_: Clock-Goto        ^ ^
     _p_: Print
     "

  ("t" org-timer-start "Start Timer")
  ("s" org-timer-stop "Stop Timer")
  ;; This one requires you be in an orgmode doc, as it sets the timer for the header
  ("r" org-timer-set-timer "Set Timer")
  ;; output timer value to buffer
  ("p" org-timer "Print Timer")
  ;; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
  ("w" (org-clock-in '(4)) "Clock-In")
  ;; you might also want (setq org-log-note-clock-out t)
  ("o" org-clock-out "Clock-Out")
  ;; global visit the clocked task
  ("j" org-clock-goto "Clock Goto")
  ;; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
  ("c" org-capture "Capture")
  ("l" org-capture-goto-last-stored "Last Capture"))

(rune/leader-keys
    "o"  '(:ignore u :which-key "org")
    "ou" '(hydra-global-org-menu/body :which-key "org global utilities"))

;; Set new tab to scratch buffer
(setq tab-bar-new-tab-choice "*scratch*")
;; (tab-bar-new-tab-to right)                ;; right is default
;; (setq tab-bar-tab-name-function tab-bar-current-tab-name)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      ;; tab-bar-button-relief               ;; controls outline of buttons
      ;; tab-bar-face tab-bar-tab            ;; configure tab face (bgcolor etc.)
      )

(setq tab-bar-show nil)                      ;; tab bar is not automatically shown (set 1 to enable)

;; Get the current tab name for use in some other display
(defun efs/current-tab-name ()
  (alist-get 'name (tab-bar--current-tab)))

(use-package windmove
  ;; :defer 4
  :config
  ;;setup default modifier key
  ;;(windmove-default-keybindings ')
)

(use-package winner-mode
  :ensure nil
  :config
  (winner-mode 1))

(use-package winum
  :config
  (winum-mode)
  ;; :bind
  ;; ("M-1" . winum-select-window-1)
  )

;; (use-package pdf-tools
;;   :pin manual
;;   :init (pdf-tools-install)
;;   :bind (:map pdf-view-mode-map
;;                           ("T" . pdf-annot-add-text-annotation)
;;                           ("D" . pdf-annot-delete)
;;                           ("t" . pdf-annot-add-highlight-markup-annotation)
;;                           ("j" . image-next-line)
;;                           ("k" . image-previous-line)
;;                           ("l" . image-forward-hscroll)
;;                           ("h" . image-backward-hscroll)
;;                           ("G" . pdf-view-last-page)
;;                           ("g" . nil)
;;                           ("gg" . pdf-view-first-page)
;;                           ("C-c C-c" . image-toggle-display)
;;                           ("C-s" . isearch-forward))
;;   :config
;;   (setq-default pdf-view-display-size 'fit-page)
;;   :custom
;;   (yas-minor-mode nil)
;;   (pdf-cache-image-limit 32)
;;   (pdf-view-max-image-width 2048)
;;   (pdf-view-resize-factor 1.8)
;;   (pdf-isearch-batch-mode t)
;;   (pdf-annot-activate-created-annotations t))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (turn-on-org-cdlatex))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
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
        '("~/Org/personal"
          "~/Org/journal"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
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

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (latex . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("se" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sp" . "src python"))
(add-to-list 'org-structure-template-alist '("sq" . "src sql"))

;;;; capture
(use-package org-capture
  :ensure nil
  :config
  (setq org-capture-templates
        `(("a" "Acronyms")

          ("ag" "General Acronyms")
          ("agg" "General Acronyms - General" table-line
           (file+olp "~/Org/personal/Acronyms.org" "General"
                     "General")
           ,(concat "\n"
                    "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|"
                    "\n %?"))
          ("agt" "General Acronyms - Terminology" table-line
           (file+olp "~/Org/personal/Acronyms.org" "General"
                     "Terminology")
           ,(concat "\n"
                    "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION}|"
                    "\n %?"))

          ("ai" "IT related Acronyms")
          ("aie" "IT related Acronyms - Emacs" table-line
           (file+olp "~/Org/personal/Acronyms.org" "IT"
                     "Emacs")
           ,(concat "\n"
                    "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |"
                    "\n %?"))
          ("ail" "IT related Acronyms - LaTeX" table-line
           (file+olp "~/Org/personal/Acronyms.org" "IT"
                     "LaTeX")
           ,(concat "\n"
                    "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |"
                    "\n %?"))
          ("aim" "IT related Acronyms - Mail" table-line
           (file+olp "~/Org/personal/Acronyms.org" "IT"
                     "Mail")
           "| %^{ACRONYM} | %^{DEFINITION} | %^{DESCRIPTION} |")

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Org/journal/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Org/journal/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)


          ("t" "Tasks / Projects")
          ("tt" "TODO Task" entry (file+olp "~/Org/personal/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("tb" "Basic task for future review" entry
           (file+headline "personal/Tasks.org" "Inbox")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l"))
          ("td" "Task with a due date" entry
           (file+headline "personal/Tasks.org" "Inbox")
           ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))


          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Org/journal/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)


          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Org/personal/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)


          ("e" "Email")
          ("em" "Make email note" entry
           (file+headline "personal/Tasks.org" "Mail correspondence")
           ,(concat "* TODO [#A] %:subject :mail:\n"
                    "SCHEDULED: %t\n:"
                    "PROPERTIES:\n:CONTEXT: %a\n:END:\n\n"
                    "%i%?"))))

  (setq org-capture-templates-contexts
        '(("m" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")))))
  :bind
  ("C-c c" . org-capture))

;; Source: https://stackoverflow.com/a/54251825
(defun contrib/org-capture-no-delete-windows (oldfun args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

;; Same source as above
(advice-add 'org-capture-place-template
            :around 'contrib/org-capture-no-delete-windows)

  ;; DOCT Package
(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/00_OrgFiles/EmacsConfig.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package mu4e
  :ensure nil
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Web.de"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Georg Rast")
                    (user-mail-address . "RastiBer@web.de")
                    (mu4e-sent-folder . "/web.de/Gesendet")
                    (mu4e-trash-folder . "/web.de/Papierkorb")
                    (mu4e-drafts-folder . "/web.de/Entwurf")
                    (mu4e-refile-folder . "/web.de/Archiv")
                    (mu4e-sent-messages-behavior . sent)
                    ))

          ;,(make-mu4e-context
          ;  :name "Personal"
          ;  :match-func (lambda (msg) (when msg
          ;                              (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          ;  :vars '(
          ;          (mu4e-sent-folder . "/Personal/Sent")
          ;          (mu4e-trash-folder . "/Personal/Deleted")
          ;          (mu4e-refile-folder . "/Personal/Archive")
          ;          ))
          ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.web.de"
        smtpmail-smtp-service 587
        smtpmail-stream-type  'ssl)

  ; ;; Signing messages (use mml-secure-sign-pgpmime)
  ; (setq mml-secure-openpgp-signers '("53C41E6E41AAFE55335ACA5E446A2ED4D940BF14"))

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/web.de/INBOX"       . ?i)
          ("/web.de/Lists/*"     . ?l)
          ("/web.de/Sent Mail"   . ?s)
          ("/web.de/Trash"       . ?t)))

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "maildir:/web.de/INBOX OR maildir:/Personal/Inbox"
                :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq dw/mu4e-inbox-query
        "(maildir:/Personal/Inbox OR maildir:/web.de/INBOX) AND flag:unread")

  (defun dw/go-to-inbox ()
    (interactive)
    (mu4e-headers-search dw/mu4e-inbox-query))

  (dw/leader-key-def
    "m"  '(:ignore t :which-key "mail")
    "mm" 'mu4e
    "mc" 'mu4e-compose-new
    "mi" 'dw/go-to-inbox
    "ms" 'mu4e-update-mail-and-index)

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c s")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package latex                 ; Activates lsp for LaTeX mode
  :ensure nil
  :hook (tex-mode . lsp-deferred))
(use-package auctex)               ; Integrated environment for TeX
(use-package auctex-latexmk)       ; LatexMK support for AUCTeX
(use-package latex-extra)          ; Useful features for LaTeX-mode
(use-package cdlatex)              ; Fast input methods for LaTeX environments and math

(setq exec-path (append exec-path '("/usr/local/texlive/2021")))

(require 'tex)
(TeX-global-PDF-mode t)            ; default compiled document: pdf

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  ; (python-shell-interpreter "python3")
  (dab-python-executable "python")
  (dab-python-debugger 'debugpy)
  :config
  (require 'dab-python)
  )

(use-package lsp-pyright)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (require 'dab-node)
  (dab-node-setup)) ;; automatically installs debug node if needed

(use-package dap-java :ensure nil)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  ;;(setq explicit-zsh-args '())
  ;; Regexp to use when searching for last prompt
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

;; add 256 color support
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;; uncomment this line if you want to use zsh
  ;; (setq vterm-shell "zsh")
  ;; set maximum lines of output to be stored in RAM
  (setq vterm-max-scrollback 10000))

;; adds git related prompt elements to eshell
(use-package eshell-git-prompt)

(use-package eshell

  :config
  ;; Set the prompt theme to powerline
  (eshell-git-prompt-use-theme 'powerline))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Programming")
    (setq projectile-project-search-path '("~/Projects/Programming")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

;  (use-package forge
;    :after magit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
