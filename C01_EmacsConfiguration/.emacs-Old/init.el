;                                  _       _ _         _ 
;   ___ _ __ ___   __ _  ___ ___  (_)_ __ (_) |_   ___| |
;  / _ \ '_ ` _ \ / _` |/ __/ __| | | '_ \| | __| / _ \ |
; |  __/ | | | | | (_| | (__\__ \ | | | | | | |_ |  __/ |
;  \___|_| |_| |_|\__,_|\___|___/ |_|_| |_|_|\__(_)___|_|


;; Basic Adjustments

;;(setq inhibit-startup-message t)

(scroll-bar-mode -1)         ; Disable visible scrollbar
;;(tool-bar-mode -1)           ; Disable the toolbar
;;(tooltip-mode -1)            ; Disable tooltips
(set-fringe-mode 10)         ; Give some breathing room
;;(menu-bar-mode -1)           ; Disble the menu bar

;; set up visible bell
(setq visible-bell t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

(laod-theme 'tango-dark)
