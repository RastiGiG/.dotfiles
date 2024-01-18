;;   _____ __  __    _    ____ ____  
;;  | ____|  \/  |  / \  / ___/ ___| 
;;  |  _| | |\/| | / _ \| |   \___ \ 
;;  | |___| |  | |/ ___ \ |___ ___) |
;;  |_____|_|  |_/_/   \_\____|____/ 
;;                                   
;;                  _             _       _ _   
;;   ___  __ _ _ __| |_   _      (_)_ __ (_) |_ 
;;  / _ \/ _` | '__| | | | |_____| | '_ \| | __|
;; |  __/ (_| | |  | | |_| |_____| | | | | | |_ 
;;  \___|\__,_|_|  |_|\__, |     |_|_| |_|_|\__|
;;                    |___/                     

;; NOTE: early-init.el is generated from EmacsEarlyInit.org.
;; Please change your settings in that file
;; using Emacs and early-init.el will be generated automatically!

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar pet/emacs--file-name-handler-alist file-name-handler-alist)
(defvar pet/emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)


;; Set the actual values after startup
;; Setting garbage collection threshold (default is 800)
;; Required for speed and also LSP
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist pet/emacs--file-name-handler-alist
                  vc-handled-backends pet/emacs--vc-handled-backends)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty
;; disruptive
;;(setq comp-async-report-warnings-errors nil)

;; --------------------------------------------------------
;; Early UI Settings

;; Frame resizing and settings
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b"))

;; Startup Messages and other inhibits
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

; Type ’y’ and ’n’ instead of ’yes’ or ’no’
(setq use-short-answers t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Enable mouse events, in case needed
(setq use-dialog-box t)

;; No file dialog
(setq use-file-dialog nil)

;;;; General theme code
(defun pet/emacs-theme-environment-dark-p ()
  "Return non-nil since environment theme is dark (ALWAYS)."
  t)

(defun pet/emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`pet/emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

;; STOP BURNING MY EYES EMACS
(defun pet/emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `pet/emacs-re-enable-frame-theme'."
  (when (pet/emacs-theme-environment-dark-p)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    (add-hook 'after-make-frame-functions #'pet/emacs-re-enable-frame-theme)))

(pet/emacs-avoid-initial-flash-of-light)

;; Name of initial frame 
(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
