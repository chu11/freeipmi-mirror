(define (commit-enable-basic-mode section-name enable-basic-mode)
  (fi-set-bmc-serial-conf-conn-mode enable-basic-mode 0 0 0))

(define (commit-enable-ppp-mode section-name enable-ppp-mode)
  (fi-set-bmc-serial-conf-conn-mode 0 enable-ppp-mode 0 0))

(define (commit-enable-terminal-mode section-name enable-terminal-mode)
  (fi-set-bmc-serial-conf-conn-mode 0 0 enable-terminal-mode 0))

(define (commit-connect-mode section-name connect-mode)
  (fi-set-bmc-serial-conf-conn-mode 0 0 0 connect-mode))

(define (commit-page-blackout-interval section-name page-blackout-interval)
  (fi-set-bmc-serial-conf-page-blackout-interval page-blackout-interval))

(define (commit-call-retry-time section-name call-retry-time)
  (fi-set-bmc-serial-conf-call-retry-time call-retry-time))

(define (commit-enable-dtr-hangup section-name enable-dtr-hangup)
  (fi-set-bmc-serial-conf-ipmi-msg-comm-settings enable-dtr-hangup #f #f))

(define (commit-flow-control section-name flow-control)
  (fi-set-bmc-serial-conf-ipmi-msg-comm-settings 0 flow-control #f))

(define (commit-bit-rate section-name bit-rate)
  (fi-set-bmc-serial-conf-ipmi-msg-comm-settings 0 #f bit-rate))

(define serial-conf-keys-validator 
  '(("Enable_Basic_Mode" valid-boolean? get-boolean commit-enable-basic-mode)
    ("Enable_PPP_Mode" valid-boolean? get-boolean commit-enable-ppp-mode)
    ("Enable_Terminal_Mode" valid-boolean? get-boolean commit-enable-terminal-mode)
    ("Connect_Mode" valid-connect-mode? get-connect-mode commit-connect-mode)
    ("Page_Blackout_Interval" valid-integer? get-integer commit-page-blackout-interval)
    ("Call_Retry_Time" valid-integer? get-integer commit-call-retry-time)
    ("Enable_DTR_Hangup" valid-boolean? get-boolean commit-enable-dtr-hangup)
    ("Flow_Control" valid-flow-control? get-flow-control commit-flow-control)
    ("Bit_Rate" valid-bit-rate? get-bit-rate commit-bit-rate)))
