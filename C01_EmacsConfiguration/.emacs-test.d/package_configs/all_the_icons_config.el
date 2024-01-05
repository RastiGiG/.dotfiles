;; Use all-the-icons
;; required for doom modeline
(use-package all-the-icons
  :if (display-graphic-p))

;; Adds icons to files and directories in dired           
(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))
