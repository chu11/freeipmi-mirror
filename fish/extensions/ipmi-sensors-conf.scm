;; ipmi-sensors-conf.scm: IPMI sensros configuration file

;; Sensors cache filename
; (set! sensors-sdr-cache-file "/path/to/cache-filename")

;; Example: Ignore these sensors
; (sensors-ignore! '(54 55 88))

;; Example: Group Aliases
(sensors-alias-list-append!
  '(
;    (mysystem . ("Processor" "Fan" "Power Supply" "Current Memory Chassis"))
    (power . ("Power Supply"))
    (security . ("Platform Chassis Intrusion" "Platform Security Violation"))
    ))
