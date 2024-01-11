;; add language-servers
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer" ))))

;; Automatically start eglot for listed languages
(cl-loop for lang in
         '(;; Python
           python-mode-hook
           python-ts-mode-hook

  		 ;; C Lang
  		 c-mode-hook
  		 c-ts-mode-hook

  		 ;; C++ Lang
  		 c++-mode-hook
  		 c++-ts-mode-hook
  		 c-or-c++-mode-hook
  		 c-or-c++-ts-mode-hook

  		 ;; Rust
  		 rust-ts-mode

  		 ;; Go
  		 go-ts-mode

  		 ;; Lua 
  		 lua-ts-mode

  		 ;; HTML
  		 html-mode
  		 html-ts-mode

  		 ;; CSS
  		 css-mode
  		 css-ts-mode

  		 ;; JSON
  		 json-ts-mode

  		 ;; YAML
           yaml-ts-mode)
         do
         (add-hook lang 'eglot-ensure))
