(define (commit-enable-basic-mode section-name enable-basic-mode)
  (if (list? enable-basic-mode)
      #t 
      (fi-set-bmc-serial-conf-conn-mode enable-basic-mode 0 0 0)))

(define (checkout-enable-basic-mode section-name)
  (let ((param-list (fi-get-bmc-serial-conf-conn-mode)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-enable-ppp-mode section-name enable-ppp-mode)
  (if (list? enable-ppp-mode)
      #t 
      (fi-set-bmc-serial-conf-conn-mode 0 enable-ppp-mode 0 0)))

(define (checkout-enable-ppp-mode section-name)
  (let ((param-list (fi-get-bmc-serial-conf-conn-mode)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-enable-terminal-mode section-name enable-terminal-mode)
  (if (list? enable-terminal-mode)
      #t 
      (fi-set-bmc-serial-conf-conn-mode 0 0 enable-terminal-mode 0)))
  
(define (checkout-enable-terminal-mode section-name) 
  (let ((param-list (fi-get-bmc-serial-conf-conn-mode)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-connect-mode section-name connect-mode)
  (if (list? connect-mode)
      #t 
      (fi-set-bmc-serial-conf-conn-mode 0 0 0 connect-mode)))

(define (checkout-connect-mode section-name) 
  (let ((param-list (fi-get-bmc-serial-conf-conn-mode))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-page-blackout-interval section-name page-blackout-interval)
  (if (list? page-blackout-interval)
      #t 
      (fi-set-bmc-serial-conf-page-blackout-interval page-blackout-interval)))

(define (checkout-page-blackout-interval section-name) 
  (fi-get-bmc-serial-conf-page-blackout-interval)) 

(define (commit-call-retry-time section-name call-retry-time)
  (if (list? call-retry-time)
      #t 
      (fi-set-bmc-serial-conf-call-retry-time call-retry-time)))

(define (checkout-call-retry-time section-name) 
  (fi-get-bmc-serial-conf-call-retry-time)) 

(define (commit-enable-dtr-hangup section-name enable-dtr-hangup)
  (if (list? enable-dtr-hangup)
      #t 
      (fi-set-bmc-serial-conf-ipmi-msg-comm-settings enable-dtr-hangup #f #f)))

(define (checkout-enable-dtr-hangup section-name) 
  (let ((param-list (fi-get-bmc-serial-conf-ipmi-msg-comm-settings))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-flow-control section-name flow-control)
  (if (list? flow-control)
      #t 
      (fi-set-bmc-serial-conf-ipmi-msg-comm-settings 0 flow-control #f)))

(define (checkout-flow-control section-name) 
  (let ((param-list (fi-get-bmc-serial-conf-ipmi-msg-comm-settings))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-bit-rate section-name bit-rate)
  (if (list? bit-rate)
      #t 
      (fi-set-bmc-serial-conf-ipmi-msg-comm-settings 0 #f bit-rate)))

(define (checkout-bit-rate section-name) 
  (let ((param-list (fi-get-bmc-serial-conf-ipmi-msg-comm-settings))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define serial-conf-keys-validator 
  '(("enable_basic_mode" 
     valid-boolean? 
     get-boolean 
     commit-enable-basic-mode 
     checkout-enable-basic-mode 
     get-boolean-string 
     "Possible values: Yes/No")
    ("enable_ppp_mode" 
     valid-boolean? 
     get-boolean 
     commit-enable-ppp-mode 
     checkout-enable-ppp-mode 
     get-boolean-string 
     "Possible values: Yes/No")
    ("enable_terminal_mode" 
     valid-boolean? 
     get-boolean 
     commit-enable-terminal-mode 
     checkout-enable-terminal-mode 
     get-boolean-string 
     "Possible values: Yes/No")
    ("connect_mode" 
     valid-connect-mode? 
     get-connect-mode 
     commit-connect-mode 
     checkout-connect-mode 
     get-connect-mode-value-string 
     "Possible Values: Modem_Connect/Direct_Connect")
    ("page_blackout_interval" 
     valid-integer? 
     get-integer 
     commit-page-blackout-interval 
     checkout-page-blackout-interval 
     any->string
     "Give valid number")
    ("call_retry_time" 
     valid-integer? 
     get-integer 
     commit-call-retry-time 
     checkout-call-retry-time 
     any->string
     "Give valid number")
    ("enable_dtr_hangup" 
     valid-boolean? 
     get-boolean 
     commit-enable-dtr-hangup 
     checkout-enable-dtr-hangup 
     get-boolean-string
     "Possible values: Yes/No")
    ("flow_control" 
     valid-flow-control? 
     get-flow-control 
     commit-flow-control 
     checkout-flow-control 
     get-flow-control-value-string
     "Possible values: No_Flow_Control/RTS_CTS/XON_XOFF")
    ("bit_rate" 
     valid-bit-rate? 
     get-bit-rate 
     commit-bit-rate 
     checkout-bit-rate 
     get-bit-rate-value-string
     "Possible values: 9600/19200/38400/57600/115200")))
