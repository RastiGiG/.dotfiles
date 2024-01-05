;; Use dired-open to launch external apps 
(use-package dired-open)
;; open .png files in 'sxiv' and .mp4 files to open in 'mpv'
;; open .pdf in 'zahtura'
(setq dired-open-extensions '(("gif" . "sxiv")
			      ("jpg" . "sxiv")
			      ("png" . "sxiv")
			      ("svg" . "sxiv")
			      ("mkv" . "mpv")
			      ("mp4" . "mpv")
			      ;; ("pdf" . "zathura") not needed with pdf-tools
			      ))

;; Add Filters by file extension to dired buffer
(use-package dired-filter)

;; Add a Preview Window to Dired
(use-package dired-preview
  :after dired
  :hook (evil-normalize-keymaps . dired-preview-hook)
  :config
  ;; Enable `dired-preview-mode' in globally:
  (dired-preview-global-mode 1)
  (setq dired-preview-delay 0.8)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp  ;; Show no preview for certain file types
        (concat "\\."
                "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
                "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
                "\\|iso\\|epub\\|pdf\\)"))
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  )
