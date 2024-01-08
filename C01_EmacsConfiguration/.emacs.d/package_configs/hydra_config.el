;; Load Hydra Package
(use-package hydra
      :config
      ;; Add leader key Menu
      (pet/leader-keys
  	"h" '(:ignore t :which-key "Hydras")))

;; Define Text Scale Hydra 
(defhydra hydra-text-scale (:timeout 4)
  "Scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(pet/leader-keys
  "hs" '(hydra-text-scale/body :which-key "Scale text"))

;; Hydra for Buffer Menu functions
(defhydra hydra-buffer-menu (
                             :hint nil
                             :timeout 10
                             ;; :color pink
                             )
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

;; Access Hydra in Buffer Menu with '.'
(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;; Bookmark Menu
(defhydra hydra-bookmark-menu (
							       :color pink
									      :hint nil
									      :timeout 10)
      "

	      ^^^Mark^             ^Actions^            ^Search^            ^Annotations^         ^Open Bookmark
	      ^^^^^^^^-----------------------------------------------------------------------------------------------------
	      _m_: mark         _x_: execute          _/_: isearch             _a_: show         _o_   on other window 
	      _u_: unmark       _r_: rename           _l_: locate              _A_: show all     _C-o_ switch other window    
	      _U_: unmark up    _R_: relocate bmk     _S_: show filenames      _e_: edit         _1_   on full window
	      _d_: delete       _w_: write bmk list   _T_: hide filenames      ^ ^               _2_   on split vertical
	      _D_: delete up    _i_: import bmk list  _t_: toggle filenames    ^ ^               _5_   on other frame
	      "
      ("m" bookmark-bmenu-mark)
      ("u" bookmark-bmenu-unmark)
      ("U" bookmark-bmenu-backup-unmark)
      ("d" bookmark-bmenu-delete)
      ("D" bookmark-bmenu-delete-backwards)
      ("x" bookmark-bmenu-execute-deletions)
      ("r" bookmark-bmenu-rename)
      ("R" bookmark-bmenu-relocate)  
      ("w" bookmark-bmenu-save)                   ;; 'write' bookmark list
      ("i" bookmark-bmenu-load)                   ;; 'import' bookmark list
      ("/" bookmark-bmenu-search)
      ("l" bookmark-bmenu-locate)
      ("S" bookmark-bmenu-show-filenames)  
      ("T" bookmark-bmenu-hide-filenames)
      ("t" bookmark-bmenu-toggle-filenames)
      ("a" bookmark-bmenu-show-annotation)
      ("A" bookmark-bmenu-show-all-annotations)
      ("e" bookmark-bmenu-edit-annotation)
      ("c" nil "cancel" :exit t)
      ("s" bookmark-bmenu-select "select" :color blue)
      ("o" bookmark-bmenu-other-window :color blue)
      ("C-o" bookmark-bmenu-switch-window :color blue)
      ("1" bookmark-bmenu-1-window :color blue)
      ("2" bookmark-bmenu-2-window :color blue)
      ("5" bookmark-bmenu-other-frame :color blue)
      ("q" quit-window "quit bm list" :color blue))

;; Access Menu through '.' in Bookmark List
(with-eval-after-load "bookmark"
      (define-key bookmark-bmenu-mode-map
			      "." 'hydra-bookmark-menu/body))

;; Apropos Hydra
(defhydra hydra-apropos (
                         ;; :color blue
                         :hint nil)
  "
^Apropos
^^^^^^^^-----------------------
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_
"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))
;; Recommended binding:
;; (global-set-key (kbd "C-c h") 'hydra-apropos/body)

;; Add to Leader keys
(pet/leader-keys
  "ha" '(hydra-apropos/body :which-key "Apropos"))

;; Window Management Helpers
(require 'windmove)

;; Move Splitter left
(defun pet/move-splitter-left (arg)
      "Move window splitter left."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'right))
	      (shrink-window-horizontally arg)
	(enlarge-window-horizontally arg)))

;; Move Splitter left
(defun pet/move-splitter-right (arg)
	"Move window splitter right."
	(interactive "p")
	(if (let ((windmove-wrap-around))
		      (windmove-find-other-window 'right))
		(enlarge-window-horizontally arg)
	      (shrink-window-horizontally arg)))

 ;; Move Splitter Up
(defun pet/move-splitter-up (arg)
      "Move window splitter up."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'up))
	      (enlarge-window arg)
	(shrink-window arg)))

;; Move Splitter Down
(defun pet/move-splitter-down (arg)
      "Move window splitter down."
      (interactive "p")
      (if (let ((windmove-wrap-around))
		(windmove-find-other-window 'up))
	      (shrink-window arg)
	(enlarge-window arg)))

;; Define Window Management Hydra
(defhydra hydra-window (
						:hint nil
							      )
      "
	Movement^^        ^Split^         ^Switch^		^Resize^
	----------------------------------------------------------------
	_M-<left>_  ←	_v_ertical    	_b_uffer		_<left>_  X←
	_M-<down>_  ↓   	_x_ horizontal	_f_ind files	_<down>_  X↓
	_M-<up>_    ↑   	_z_ undo      	_a_ce 1	    	_<up>_    X↑
	_M-<right>_ →   	_Z_ reset      	_s_wap	     	_<right>_ X→
	_F_ollow Mode    	_D_lt Other   	_S_ave	     max_i_mize
	_SPC_ cancel	    _o_nly this   	_d_elete	
	"
      ;; Movement
      ("M-<left>"  windmove-left)
      ("M-<down>"  windmove-down)
      ("M-<up>"    windmove-up)
      ("M-<right>" windmove-right)

      ;; Resize
      ("<left>"  pet/move-splitter-left)
      ("<down>"  pet/move-splitter-down)
      ("<right>" pet/move-splitter-right)
      ("<up>"    pet/move-splitter-up)

      ("b" list-buffers)
      ("f" find-files)
      ("F" follow-mode)
      ("a" (lambda ()
		 (interactive)
		 (ace-window 1)
			 (add-hook 'ace-window-end-once-hook
					       'hydra-window/body))
       )
      ("v" (lambda ()
		 (interactive)
		 (split-window-right)
		 (windmove-right))
       )
      ("x" (lambda ()
		 (interactive)
		 (split-window-below)
		 (windmove-down))
       )
      ("s" (lambda ()
		 (interactive)
		 (ace-window 4)
		 (add-hook 'ace-window-end-once-hook
				       'hydra-window/body)))
      ("S" save-buffer)
      ("d" delete-window)
      ("D" (lambda ()
		 (interactive)
		 (ace-window 16)
		 (add-hook 'ace-window-end-once-hook
				       'hydra-window/body))
       )
      ("o" delete-other-windows)
      ("i" ace-maximize-window)
      ("z" (progn
		 (winner-undo)
		 (setq this-command 'winner-undo))
       )
      ("Z" winner-redo)
      ("SPC" nil)
      )

;; Add to Leader keys
(pet/leader-keys
      "hw" '(hydra-window/body :which-key "Window Management")
      )

;; hydra multiple cursors
(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^             ^Other^
--------------------------------------------------------
[_p_]   Previous    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip        [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark      [_M-n_] Unmark  [_r_] Mark by regexp
^ ^                 ^ ^             [_d_] Mark all defun
^ ^                 ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("d" mc/mark-all-like-this-in-defun :exit t)
  ("q" nil))

;; Add to Leader keys
(pet/leader-keys
  "hm" '(hydra-multiple-cursors/body :which-key "Multiple Cursors"))

;; Editing Toggles
(defhydra hydra-editing-visuals (
						:color pink
							       :hint nil
							       )
      "
^Editing Visuals
^^^^^^-------------------------------------------------------------------------
_a_ abbrev-mode:                         %`abbrev-mode
_C_ display-fill-column-indicator-mode:  %`display-fill-column-indicator-mode
_d_ debug-on-error:                      %`debug-on-error
_f_ auto-fill-mode:                      %`auto-fill-function
_F_ variable-pitch-mode                 
_i_ toggle-input-method                 
_n_ display-line-numbers-mode:           %`display-line-numbers-mode
_M_ doom-modeline-mode:                  %`doom-modeline-mode
_R_ read-only-mode                      
_t_ truncate-lines:                      %`truncate-lines
_T_ counsel-load-theme                  
_v_ visual-line-mode:                    %`visual-line-mode
_w_ whitespace-mode:                     %`whitespace-mode
"
      ("a" abbrev-mode)
      ("C" display-fill-column-indicator-mode)
      ("d" toggle-debug-on-error)
      ("f" auto-fill-mode)
      ("F" variable-pitch-mode)
      ("i" toggle-input-method)
      ("t" toggle-truncate-lines)
      ("T" counsel-load-theme)
      ("v" visual-line-mode)
      ("n" display-line-numbers-mode)
      ("M" doom-modeline-mode)
      ("w" whitespace-mode)
      ("R" read-only-mode)
      ("q" nil "quit" :exit 1))

;; (global-set-key (kbd "C-c C-v") 'hydra-editing-toggles/body)

;; Add to Key Space
(pet/leader-keys
      "eh" '(hydra-editing-visuals/body :which-key "Editing Visuals")
      "T"  '(hydra-editing-visuals/body :which-key "Toggle Hydra")
      "ht" '(hydra-editing-visuals/body :which-key "Editing Visuals"))

;; Mu4e Hydra
(defhydra hydra-mu4e-headers (
							      :color blue
									 :hint nil
									 )
      "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups 
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------ 
_C_: compose | _}_: next query    | _a_: action  | _|_: to shell  | _´_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_h_: ?mode   | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

      ;; general
      ("n" mu4e-headers-next)
      ("p" mu4e-headers-previous)
      ("[" mu4e-select-next-unread)
      ("]" mu4e-select-previous-unread)
      ("y" mu4e-select-other-view)
      ("R" mu4e-compose-reply)
      ("C" mu4e-compose-new)
      ("F" mu4e-compose-forward)

      ;; search
      ("s" mu4e-headers-search)
      ("S" mu4e-headers-search-edit)
      ("/" mu4e-headers-search-narrow)
      ("b" mu4e-headers-search-bookmark)
      ("B" mu4e-headers-search-bookmark-edit)
      ("{" mu4e-headers-search-prev :color pink)      ; differs from built-in - make sure to add them later
      ("}" mu4e-headers-search-next :color pink)      ; differs from built-in - make sure to add them later
      ("C-+" mu4e-headers-split-view-grow)
      ("C--" mu4e-headers-split-view-shrink)

      ;; mark stuff 
      ("!" mu4e-headers-mark-for-read)
      ("?" mu4e-headers-mark-for-unread)
      ("r" mu4e-headers-mark-for-refile)
      ("u" mu4e-headers-mark-for-unmark)
      ("U" mu4e-mark-unmark-all)
      ("d" mu4e-headers-mark-for-trash)
      ("D" mu4e-headers-mark-for-delete)
      ("m" mu4e-headers-mark-for-move)
      ("a" mu4e-headers-action)                  ; not really a mark per-se
      ("A" mu4e-headers-mark-for-action)
      ("*" mu4e-headers-mark-for-something)


      ("#" mu4e-mark-resolve-deferred-marks)
      ("%" mu4e-headers-mark-pattern)
      ("&" mu4e-headers-mark-custom)
      ("+" mu4e-headers-mark-for-flag)
      ("-" mu4e-headers-mark-for-unflag)
      ("t" mu4e-headers-mark-subthread)
      ("T" mu4e-headers-mark-thread)

      ;; miscellany
      ("q" mu4e~headers-quit-buffer)
      ("H" mu4e-display-manual)
      ("h" describe-mode)
      ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

      ;; switches
      ("O" mu4e-headers-change-sorting)
      ("P" mu4e-headers-toggle-threading)
      ("Q" mu4e-headers-toggle-full-search)
      ("V" mu4e-headers-toggle-skip-duplicates)
      ("W" mu4e-headers-toggle-include-related)

      ;; more miscellany
      ("´" mu4e-update-mail-and-index)           ; differs from built-in
      (";" mu4e-context-switch)  
      ("j" mu4e~headers-jump-to-maildir)

      ("." nil))

;; Add Indent Tools for languages with identation based structures
(use-package indent-tools
      :hook
      (python-mode .  (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body)))
      (yaml-mode .  (lambda () (define-key yaml-mode-map (kbd "C-c >") 'indent-tools-hydra/body))))
