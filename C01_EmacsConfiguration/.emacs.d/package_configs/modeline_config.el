;; Load doom modeline
(use-package doom-modeline
  :after (all-the-icons)
  ;; Activate Doom Modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35                   ;; sets modeline height
        doom-modeline-bar-width 5                 ;; sets right bar width
        doom-modeline-persp-name t                ;; adds perspective name to modeline
        doom-modeline-window-width-limit nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-minor-modes nil             ;; don’t try to display minor mode names
        doom-modeline-enable-word-count t         ;; display word count
        doom-modeline-buffer-encoding t           ;; show encoding for current buffer
        doom-modeline-buffer-modification-icon t  ;; indicate modification
        doom-modeline-env-python-executable "python"
        ;; needs display-time-mode to be one
        doom-modeline-time t
        doom-modeline-vcs-max-length 12           ;; length of VCS branch name displayed - default 12
        doom-modeline-persp-icon t))              ;; adds folder icon next to persp name
