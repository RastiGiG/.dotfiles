;; Define Servers
(defun pet/irc-libera-server
  	(interactive)
      (erc-tls :server "irc.libera.chat"
  		       :port   "6697"))

(defun pet/irc-hackint-server
  	(interactive)
      (erc-tls :server "irc.hackint.org"
  		       :port   "6697"))

(defun pet/irc-hackint-de-server
  	(interactive)
      (erc-tls :server "irc.hackint.org"
  		       :port   "6697"))

(defun pet/irc-oftc-server
  	(interactive)
      (erc-tls :server "irc.oftc.net"
  		       :port   "6697"))

;; Setup ERC Chat Client
;; Set the Prompt to represent the the buffer-name 
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))

  	;; Basic Account Config
  	;; Default Server
  	erc-server "irc.libera.chat"
  	erc-nick "sailti"

  	;; More info on the modeline
  	erc-track-shorten-start 8

  	;; cleanup buffers
  	erc-kill-buffer-on-part t

  	;; channel list
  	erc-autojoin-channel-alist
  	'(("irc.libera.chat"
  	       "#systemcrafters"
  	       "#emacs"))

  	;; bury private messages in buffer list
  	erc-auto-query 'bury

  	;; Autofill nickname column to 20 chars for better formatting 
  	erc-fill-function 'erc-fill-static
  	erc-fill-static-center 30)

;; Set Keyboard to be accessable by 'C-c i' 
(global-set-key (kbd "C-c i") 'erc-tls)

(pet/leader-keys
      "i"  '(:ignore t :which-key "IRC")
      "mi" 'erc-tls
      "ml" '(pet/irc-libera-server :which-key "Libera Chat")
      "mh" '(pet/irc-hackint-server :which-key "Hack Int")
      "mo" '(pet/irc-oftc-server :which-key "Open and Free Technology Community"))
