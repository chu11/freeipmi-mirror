(define (commit-volatile-access-mode section-name access-mode)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-volatile-access access-mode 0 0 0 #f)
      (fi-set-bmc-lan-channel-volatile-access access-mode 0 0 0 #f)))

(define (commit-volatile-enable-user-level-auth section-name enable-user-level-auth)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-volatile-access #f enable-user-level-auth 0 0 #f)
      (fi-set-bmc-lan-channel-volatile-access #f enable-user-level-auth 0 0 #f)))

(define (commit-volatile-enable-per-message-auth section-name enable-per-message-auth)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-volatile-access #f 0 enable-per-message-auth 0 #f)
      (fi-set-bmc-lan-channel-volatile-access #f 0 enable-per-message-auth 0 #f)))

(define (commit-volatile-enable-pef-alerting section-name enable-pef-alerting)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-volatile-access #f 0 0 enable-pef-alerting #f)
      (fi-set-bmc-lan-channel-volatile-access #f 0 0 enable-pef-alerting #f)))

(define (commit-volatile-channel-privilege-limit section-name channel-privilege-limit)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-volatile-access #f 0 0 0 channel-privilege-limit)
      (fi-set-bmc-lan-channel-volatile-access #f 0 0 0 channel-privilege-limit)))

(define (commit-non-volatile-access-mode section-name access-mode)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-non-volatile-access access-mode 0 0 0 #f)
      (fi-set-bmc-lan-channel-non-volatile-access access-mode 0 0 0 #f)))

(define (commit-non-volatile-enable-user-level-auth section-name enable-user-level-auth)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-non-volatile-access #f enable-user-level-auth 0 0 #f)
      (fi-set-bmc-lan-channel-non-volatile-access #f enable-user-level-auth 0 0 #f)))

(define (commit-non-volatile-enable-per-message-auth section-name enable-per-message-auth)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-non-volatile-access #f 0 enable-per-message-auth 0 #f)
      (fi-set-bmc-lan-channel-non-volatile-access #f 0 enable-per-message-auth 0 #f)))

(define (commit-non-volatile-enable-pef-alerting section-name enable-pef-alerting)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-non-volatile-access #f 0 0 enable-pef-alerting #f)
      (fi-set-bmc-lan-channel-non-volatile-access #f 0 0 enable-pef-alerting #f)))

(define (commit-non-volatile-channel-privilege-limit section-name channel-privilege-limit)
  (if (string-ci=? section-name "serial_channel")
      (fi-set-bmc-serial-channel-non-volatile-access #f 0 0 0 channel-privilege-limit)
      (fi-set-bmc-lan-channel-non-volatile-access #f 0 0 0 channel-privilege-limit)))

(define lan-serial-channel-keys-validator 
  '(("volatile_access_mode" valid-channel-access-mode? get-channel-access-mode commit-volatile-access-mode)
    ("volatile_enable_user_level_auth" valid-boolean? get-boolean commit-volatile-enable-user-level-auth)
    ("volatile_enable_per_message_auth" valid-boolean? get-boolean commit-volatile-enable-per-message-auth)
    ("volatile_enable_pef_alerting" valid-boolean? get-boolean commit-volatile-enable-pef-alerting)
    ("volatile_channel_privilege_limit"	valid-privilege-limit? get-privilege-limit commit-volatile-channel-privilege-limit)
    ("non_volatile_access_mode" valid-channel-access-mode? get-channel-access-mode commit-non-volatile-access-mode)
    ("non_volatile_enable_user_level_auth" valid-boolean? get-boolean commit-non-volatile-enable-user-level-auth)
    ("non_volatile_enable_per_message_auth" valid-boolean? get-boolean commit-non-volatile-enable-per-message-auth)
    ("non_volatile_enable_pef_alerting" valid-boolean? get-boolean commit-non-volatile-enable-pef-alerting)
    ("non_volatile_channel_privilege_limit" valid-privilege-limit? get-privilege-limit commit-non-volatile-channel-privilege-limit)
    ;; You can add more in the form of 
    ;; (KEYSTRING VALIDATION-PROC CONVERTION-PROC BMC-COMMIT-PROC)
    ))

