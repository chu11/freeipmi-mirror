;; fish.scm: default fish configuration scm

;; Customize Fish:
(fi-set-prompt! "fish# ")

;; Set driver SMS IO Base port
; (fi-set-sms-io-base! #x0CA2)

;; Set Driver Internals:
; (fi-set-default-driver-poll-interval 10)

;; Example Group Aliases
; (set! sensors-group-alias-list
;   '(
;     (mysystem . (Processor Fan "Power Supply" Current Memory Chassis))
;     (power . ("Power Supply"))
;     (security . ("Platform Chassis Intrusion Platform Security Violation"))
;     ))
