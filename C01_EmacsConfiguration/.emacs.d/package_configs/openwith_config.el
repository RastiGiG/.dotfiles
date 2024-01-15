;; Add Openwidth to use more sensible programs for certain file
(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))

         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))

         '("\\.lyx" "lyx" (file))

         '("\\.chm" "kchmviewer" (file))

         (list (openwith-make-extension-regexp
                '("ps" "ps.gz" "dvi"))
               "okular"
               '(file))

         (list (openwith-make-extension-regexp
                '("kdenlive"))
               "kdenlive-accel"
               '(file))

         (list (openwith-make-extension-regexp
                '("kra"))
               "krita"
               '(file))

         (list (openwith-make-extension-regexp
                '("blend" "blend1"))
               "blender"
               '(file))

         (list (openwith-make-extension-regexp
                '("helio"))
               "helio"
               '(file))

         (list (openwith-make-extension-regexp
                '("svg"))
               "inkscape"
               '(file))

         (list (openwith-make-extension-regexp
                '("flp"))
               "~/.local/bin/flstudio"
               '(file))
         ))
  (setq openwith-confirm-invocation t)  ;; Avoid starting other processes by accident
  ;; Activate openwidth mode
  (openwith-mode 1))
