;; Set Leader Keys
(pet/leader-keys

  ;; Layouts
  "l"     '(:ignore t :which-key "Layout")
  "lw"    '(winner-undo :which-key "Winner Undo")


  ;; Authentication
  "a"     '(:ignore t :which-key "Authentification")


  ;; Bookmarks
  "b"     '(:ignore t :which-key "Bookmarks")
  "bs"    '(bookmark-set :which-key "Set Bookmark")
  "bl"    '(bookmark-bmenu-list :which-key "Bookmark List")
  "bt"    '(pet/current-tab-name :which-key "Current Tab Name")
  "bw"    '(webjump :which-key "Webjump to bookmark")

  ;; Calculator
  "c"   '(calc :which-key "Calculator")

  ;; Editing Tools
  "e"     '(:ignore t :which-key "Editing Tools")
  "ea"    'add-file-local-variable-prop-line
  ;; Letters
  "el"    '(:ignore t :which-key "Letters")
  "elM-u" 'upcase-initials
  "elC-uM-u" 'upcase-initials-region
  ;; Tabs
  "et"    '(untabify
      	      :which-key "Untabify")
  "er"    '(regexp-builder
      	      :which-key "Regexp Builder")


  ;; Files
  "f"     '(:ignore t :which-key "Files")
  "fR"    'recentf-open-files


  ;; Org Mode related
  "o"     '(:ignore t :which-key "Org Mode")


  ;; Toggles
  "t"     '(:ignore t :which-key "Toggles")
  "tc"    'world-clock
  "tt"    '(counsel-load-theme
      	      :which-key "Choose Theme")
  ;; Toggles - Highlighting
  "th"    '(:ignore t :which-key "Highlighting")
  ;; Toggles - Highlighting - Colors
  "thc"   '(:ignore t :which-key "Colors")
  "thcr"  '(pet/syntax-color-rgb
      	      :which-key "RGB")
  "thch"  '(pet/syntax-color-hsv
      	      :which-key "HSV")
  ;; Toggles - Language Server
  "tl"    '(:ignore t :which-key "LSP") 
  "tls"   '(eglot :which-key "Start LSP with Eglot")
  ;; Toggles - Modes
  "tm"    '(:ignore t :which-key "Modes")
  "tmv"   '(:ignore t :which-key "Modes with v..")
  "tmvl"  '(visual-line-mode :which-key "Visual Line Mode")
  "tmh"   '(hl-line-mode :which-key "Highlight Line Mode")
  "tmw"   '(whitespace-mode :which-key "Whitspace Mode")
  "tmu"   '(undo-tree-mode :which-key "Undo-Tree Mode")
  "tmo"   '(org-mode :which-key "Org Mode")
  "tmf"   '(origami-mode :which-key "Origami Mode")
  "tmf"   '(follow-mode :which-key "Follow Mode")
  "tme"   '(emojify-mode :which-key "Emojify Mode")
  "tms"   '(scroll-all-mode :which-key "Scroll All Mode")

  ;; Tools
  "T"       '(:ignore t :which-key "Tools")

  ;; Tools - Dictionary
  "Td"      '(:ignore t :which-key "Dictionary")
  "Tdd"     '(dictionary :which-key "Open Dictionary")
  "Tdl"     '(dictionary-lookup-definition :which-key "Lookup Definition")
  "Tdm"     '(dictionary-match-words :which-key "Match Words")
  "Tds"     '(dictionary-search :which-key "Search Dictionary"))
