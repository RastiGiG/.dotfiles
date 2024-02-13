;;  _____ _   _ _   _  ____ _____ ___ ___  _   _ ____
;; |  ___| | | | \ | |/ ___|_   _|_ _/ _ \| \ | / ___|
;; | |_  | | | |  \| | |     | |  | | | | |  \| \___ \
;; |  _| | |_| | |\  | |___  | |  | | |_| | |\  |___) |
;; |_|    \___/|_| \_|\____| |_| |___\___/|_| \_|____/
;;

;; Returns the color substring from given range
(defun pet/substring-from-range (str range)
  "Return substring from a given STR by specified RANGE"
  (substring str (first range) (second range)))

;; Return a range for specified colorchannel in a 6-digit
;; hexnumber
(defun pet/colorchannel-into-range (color)
  "Returns the range to look for a specified color.
  Inputs must be in 'rgbRGB' "
  (setq color (s-lower-camel-case color))
  (cond ((equal color "r") '(1 3))
    ((equal color "g") '(3 5))
    ((equal color "b") '(5 7))))

;; Return the substring for a specified
(defun pet/colorsubstr-from-colorstr (colorstr colorchannel)
  "Returns the channelstr of the specified colorchannel from
   colorstrings like「#011f00」"
  (setq range (pet/colorchannel-into-range colorchannel))
  (pet/substring-from-range colorstring range))

;; Return the value of a specified colorchannel
(defun pet/number-from-string-by-channel
    (colorstring colorchannel)
  "Returns the numeric value of the specified colorchannel from
   colorstrings like「#011f00」"
  (string-to-number
  (pet/colorsubstr-from-colorstr
   (colorstr colorchannel)) 16))

;; Calculate Color average across channels from colorstr
(defun pet/avg-color (color)
  "Calculates the Color Average from COLOR"
  (/ (+ (pet/number-from-string-by-channel color "r")
    (pet/number-from-string-by-channel color "g")
    (pet/number-from-string-by-channel color "b"))
     3))

;; Returns a Color that contrasts background
(defun pet/contrast-color (bg-avg-decimal)
  "Returns the foreground color based on the avg background
  being below 128. Returns White 「#000000」 if average is
  above"
  (if (> bg-avg-decimal 128) "#000000" "#ffffff"))

;; Takes a color string like #ffe0e0 and returns a light
;; or dark foreground color to make sure text is readable.
(defun pet/fg-from-bg (bg)
  "Returns the foreground color based on the avg background
   being below 128. Returns White 「#000000」 if average is
   above"
  (setq avg (pet/avg-color bg))
  (pet/contrast-color avg))

;; Function to Color Hexstring with their corresponding Colors
;; in RGB format
(defun pet/syntax-color-rgb ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in
  current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef0-9]\\{3\\}[^ABCDEFabcdef0-9]"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list
		 :background (let* (
		  (ms (match-string-no-properties 0))
		  (r (pet/colorsubstr-from-colorstr bgstr "r"))
		  (g (pet/colorsubstr-from-colorstr bgstr "g"))
		  (b (pet/colorsubstr-from-colorstr bgstr "b"))
			     )
		  (concat "#" r r g g b b))))))
     ("#[ABCDEFabcdef0-9]\\{6\\}"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background
		      (match-string-no-properties 0)))))))
  (font-lock-flush))

;; Function to Colorstring with their corresponding Colors
;; in HSL format
(defun pet/syntax-color-hsl ()
  "Syntax color CSS's HSL color spec eg 「hsl(0,90%,41%)」 in
  current buffer."
  (interactive)
  (require 'color)
  (font-lock-add-keywords
   nil
   '(("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *,
    *\\([0-9]\\{1,3\\}\\)% *)"
  (0 (put-text-property
      (+ (match-beginning 0) 3)
      (match-end 0)
      'face
      (list
       :background
       (concat
        "#"
        (mapconcat
         'identity
         (mapcar
      (lambda (x) (format "%02x" (round (* x 255))))
      (color-hsl-to-rgb
       (/ (string-to-number (match-string-no-properties 1)) 360.0)
       (/ (string-to-number (match-string-no-properties 2)) 100.0)
       (/ (string-to-number (match-string-no-properties 3)) 100.0)))
         "" )) ;  "#00aa00"
       ))))))
  (font-lock-flush))

;; Function to insert a random color in HSL format
(defun pet/insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
  Sample output: hsl(100,24%,82%);"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);"
          (random 360) (random 100) (random 100))))

;; Function to check for file existence before loading
(defun pet/load-file (file)
  (when (file-exists-p file)
    (load file)))

;; Function to check for font availability
(defun pet/font-available-p (font-name)
      (find-font (font-spec :name font-name)))

;; Custom window split function
(defun pet/split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

;; Add user-facing command for dedicated windows
(defun pet/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
  						(not (window-dedicated-p (selected-window)))))

;; Check if a buffer major mode is derived from a list of given major modes
(defun pet/make-display-buffer-matcher-function (major-modes)
  (lambda (buffer-name action)
    (with-current-buffer buffer-name (apply #'derived-mode-p major-modes))))

;; Check if a buffer is associated to a project
(defun pet/mp-buffer-has-project-p (buffer action)
  (with-current-buffer buffer (project-current nil)))

;; Join a line by separator
(defun pet/join-lines (specify-separator)
      "Join lines in the active region by a separator, by default a comma.
Specify the separator by typing C-u before executing this command.

Note: it depends on s.el."
      (interactive "P")
      (require 's)
      (unless (region-active-p)
	(message "select a region of lines first."))
      (let* ((separator (if (not specify-separator)
						","
					      (read-string "Separator: ")))
		 (text (buffer-substring-no-properties
			       (region-beginning)
			       (region-end)))
		 (lines (split-string text "\n"))
		 (result (s-join separator lines)))
	(delete-region (region-beginning) (region-end))
	(insert result)))

;; Highlight text in org mode with mouse
(define-advice mouse-set-region (:after (click) org-highlight ())
      (when (and (derived-mode-p 'org-mode)
			 (use-region-p))
      (let ((origin (buffer-substring (region-beginning) (region-end)))
		(emphasis-char "*"))
	(delete-region (region-beginning) (region-end))
	(insert emphasis-char origin emphasis-char))))

(defun pet/check-native-compilation-p ()
    (if (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
        (message "Native compilation is available")
      (message "Native complation is *not* available")))

(defun pet/check-native-json-p ()
    (if (functionp 'json-serialize)
        (message "Native JSON is available")
      (message "Native JSON is *not* available")))

;; Custom function to calculate the time balance
(require 'cl-lib)
(require 'org-clock)

(defun org-dblock-write:track-weekly-overtime (params)
  "Calculate how many hours too many or too few I have worked. PARAMS are
defined in the template, they are :tstart for the first day for which there's
data (e.g. <2022-01-01>), :tend for the last date (e.g. <now>) and :hours for
amount of hours per day (defaults to 8 if not specified)."
  ;; cl-flet is a macro from the common lisp emulation package that allows us to
  ;; bind functions, just like let allows us to do with values.
  (cl-flet* ((format-time (time)
  			 (format-time-string (org-time-stamp-format t t) time))
  		       (get-minutes-from-log (t1 t2)
  			 (cl-second (org-clock-get-table-data
  						 (buffer-file-name)
  						 (list :maxlevel 0
  							       :tstart (format-time t1)
  							       :tend (format-time t2))))))
    (let* ((start
            (seconds-to-time (org-matcher-time (plist-get params :tstart))))
           (end
            (seconds-to-time (org-matcher-time (plist-get params :tend))))
  		 (time start)
           (total-days-worked 0))
      (progn
        ;; loop through all the days in the time frame provided and count how
        ;; many days minutes were reported.
        (while (time-less-p time end)
          (let* ((next-day (time-add time (date-to-time "1970-01-02T00:00Z")))
                 (minutes-in-day (get-minutes-from-log time next-day)))
            (if (> minutes-in-day 0) (cl-incf total-days-worked 1))
            (setq time next-day)))
        ;; now we can just do some simple arithmetic to get the difference
        ;; between hours ideally worked and hours actually worked.
        (let* ((total-minutes-worked (get-minutes-from-log start end))
               (hours-worked (/ total-minutes-worked 60.0))
               (hours-per-workday (or (plist-get params :hours) 8))
               (hours-should-work (* total-days-worked hours-per-workday))
               (hour-difference (- hours-worked hours-should-work)))
          (insert (format "%0.1f" hour-difference)))))))

;; Custom function to calculate the time balance
(require 'cl-lib)
(require 'org-clock)

(defun org-dblock-write:track-monthly-overtime (params)
  "Calculate how many hours too many or too few I have worked. PARAMS are
defined in the template, they are :tstart for the first day for which there's
data (e.g. <2022-01-01>), :tend for the last date (e.g. <now>) and :hours for
amount of hours per month (defaults to 8 if not specified)."
  ;; cl-flet is a macro from the common lisp emulation package that allows us to
  ;; bind functions, just like let allows us to do with values.
  (cl-flet* ((format-time (time)
  			 (format-time-string (org-time-stamp-format t t) time))
  		       (get-minutes-from-log (t1 t2)
  			 (cl-second (org-clock-get-table-data
  						 (buffer-file-name)
  						 (list :maxlevel 0
  							       :tstart (format-time t1)
  							       :tend (format-time t2))))))
    (let* ((start
            (seconds-to-time (org-matcher-time (plist-get params :tstart))))
           (end
            (seconds-to-time (org-matcher-time (plist-get params :tend))))
  		 (time start)
           (total-days-worked 0))
      (progn
        ;; loop through all the days in the time frame provided and count how
        ;; many days minutes were reported.
        (while (time-less-p time end)
          (let* ((next-day (time-add time (date-to-time "1970-01-02T00:00Z")))
                 (minutes-in-day (get-minutes-from-log time next-day)))
            (setq time next-day)))
        ;; now we can just do some simple arithmetic to get the difference
        ;; between hours ideally worked and hours actually worked.
        (let* ((total-minutes-worked (get-minutes-from-log start end))
               (hours-worked (/ total-minutes-worked 60.0))
               (hours-per-month (or (plist-get params :hours) 160))
               (hours-should-work hours-per-month)
               (hour-difference (- hours-worked hours-should-work)))
          (insert (format "%0.1f" hour-difference)))))))

(defun pet/org-agenda-open-hook ()
  "Hook for org-agenda using visual-fill-column"
(visual-fill-column-mode))

;; This function grabs a list of org files from the specified directories
;; I use this function in my custom time table to track worktime across different files
(defun pet/collect-org-files ()
  (let ((dir1 (directory-files-recursively "~/Projects/Writing" ".org"))
        (dir2 (directory-files-recursively "~/Projects/Programming" ".org"))
  	      (dir3 (directory-files-recursively "~/Projects/Servers" ".org")))
    (setq result (nconc dir1 dir2 dir3))))
