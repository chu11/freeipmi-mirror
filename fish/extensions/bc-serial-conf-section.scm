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
  '(("enable_basic_mode" valid-boolean? get-boolean commit-enable-basic-mode)
    ("enable_ppp_mode" valid-boolean? get-boolean commit-enable-ppp-mode)
    ("enable_terminal_mode" valid-boolean? get-boolean commit-enable-terminal-mode)
    ("connect_mode" valid-connect-mode? get-connect-mode commit-connect-mode)
    ("page_blackout_interval" valid-integer? get-integer commit-page-blackout-interval)
    ("call_retry_time" valid-integer? get-integer commit-call-retry-time)
    ("enable_dtr_hangup" valid-boolean? get-boolean commit-enable-dtr-hangup)
    ("flow_control" valid-flow-control? get-flow-control commit-flow-control)
    ("bit_rate" valid-bit-rate? get-bit-rate commit-bit-rate)))
