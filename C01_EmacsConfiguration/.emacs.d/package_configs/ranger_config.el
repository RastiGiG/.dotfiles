;; Add Ranger Directory Explorer
(use-package ranger
  :config
  ;; I don't want ranger to be the default
  (setq ranger-override-dired-mode nil)
  ;; Enable Image preview
  (setq ranger-show-literal nil)
  ;; Set Max Preview Size to 50MB
  ;; !!careful, this can really slow down your machine!!
  (setq ranger-max-preview-size 50)
  ;; Don't preview video/audio files
  (setq ranger-excluded-extensions ' ("mkv" "iso" "mp4" "mp3"))
  (pet/leader-keys
    "tmr"  '(ranger-mode :which-key "Ranger Mode")))
