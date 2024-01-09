;; add language-servers
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer" ))))
