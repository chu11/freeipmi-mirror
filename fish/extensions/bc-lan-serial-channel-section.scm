(define (commit-volatile-access-mode section-name access-mode)
  (if (list? access-mode)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-volatile-access access-mode 0 0 0 #f)
	  (fi-set-bmc-lan-channel-volatile-access access-mode 0 0 0 #f))))

(define (checkout-volatile-access-mode section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-volatile-access) 
			(fi-get-bmc-lan-channel-volatile-access)))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-volatile-enable-user-level-auth section-name enable-user-level-auth)
  (if (list? enable-user-level-auth)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-volatile-access #f enable-user-level-auth 0 0 #f)
	  (fi-set-bmc-lan-channel-volatile-access #f enable-user-level-auth 0 0 #f))))

(define (checkout-volatile-enable-user-level-auth section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-volatile-access) 
			(fi-get-bmc-lan-channel-volatile-access))))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-volatile-enable-per-message-auth section-name enable-per-message-auth)
  (if (list? enable-per-message-auth)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-volatile-access #f 0 enable-per-message-auth 0 #f)
	  (fi-set-bmc-lan-channel-volatile-access #f 0 enable-per-message-auth 0 #f))))

(define (checkout-volatile-enable-per-message-auth section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-volatile-access) 
			(fi-get-bmc-lan-channel-volatile-access)))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-volatile-enable-pef-alerting section-name enable-pef-alerting)
  (if (list? enable-pef-alerting)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-volatile-access #f 0 0 enable-pef-alerting #f)
	  (fi-set-bmc-lan-channel-volatile-access #f 0 0 enable-pef-alerting #f))))

(define (checkout-volatile-enable-pef-alerting section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel")
			(fi-get-bmc-serial-channel-volatile-access) 
			(fi-get-bmc-lan-channel-volatile-access)))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-volatile-channel-privilege-limit section-name channel-privilege-limit)
  (if (list? channel-privilege-limit)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-volatile-access #f 0 0 0 channel-privilege-limit)
	  (fi-set-bmc-lan-channel-volatile-access #f 0 0 0 channel-privilege-limit))))

(define (checkout-volatile-channel-privilege-limit section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel")
			(fi-get-bmc-serial-channel-volatile-access) 
			(fi-get-bmc-lan-channel-volatile-access))))  
    (if (list? param-list) (cddddr param-list) #f)))

(define (commit-non-volatile-access-mode section-name access-mode)
  (if (list? access-mode)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-non-volatile-access access-mode 0 0 0 #f)
	  (fi-set-bmc-lan-channel-non-volatile-access access-mode 0 0 0 #f))))

(define (checkout-non-volatile-access-mode section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-non-volatile-access) 
			(fi-get-bmc-lan-channel-non-volatile-access)))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-non-volatile-enable-user-level-auth section-name enable-user-level-auth)
  (if (list? enable-user-level-auth)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-non-volatile-access #f enable-user-level-auth 0 0 #f)
	  (fi-set-bmc-lan-channel-non-volatile-access #f enable-user-level-auth 0 0 #f))))

(define (checkout-non-volatile-enable-user-level-auth section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-non-volatile-access) 
			(fi-get-bmc-lan-channel-non-volatile-access)))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-non-volatile-enable-per-message-auth section-name enable-per-message-auth)
  (if (list? enable-per-message-auth)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-non-volatile-access #f 0 enable-per-message-auth 0 #f)
	  (fi-set-bmc-lan-channel-non-volatile-access #f 0 enable-per-message-auth 0 #f))))

(define (checkout-non-volatile-enable-per-message-auth section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel")
			(fi-get-bmc-serial-channel-non-volatile-access) 
			(fi-get-bmc-lan-channel-non-volatile-access)))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-non-volatile-enable-pef-alerting section-name enable-pef-alerting)
  (if (list? enable-pef-alerting)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-non-volatile-access #f 0 0 enable-pef-alerting #f)
	  (fi-set-bmc-lan-channel-non-volatile-access #f 0 0 enable-pef-alerting #f))))

(define (checkout-non-volatile-enable-pef-alerting section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel") 
			(fi-get-bmc-serial-channel-non-volatile-access) 
			(fi-get-bmc-lan-channel-non-volatile-access)))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-non-volatile-channel-privilege-limit section-name channel-privilege-limit)
  (if (list? channel-privilege-limit)
      #t 
      (if (string-ci=? section-name "serial_channel")
	  (fi-set-bmc-serial-channel-non-volatile-access #f 0 0 0 channel-privilege-limit)
	  (fi-set-bmc-lan-channel-non-volatile-access #f 0 0 0 channel-privilege-limit))))

(define (checkout-non-volatile-channel-privilege-limit section-name) 
  (let ((param-list (if (string-ci=? section-name "serial_channel")
			(fi-get-bmc-serial-channel-non-volatile-access) 
			(fi-get-bmc-lan-channel-non-volatile-access)))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define lan-serial-channel-keys-validator 
  '(
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERTION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERTION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ("volatile_access_mode" 
     valid-channel-access-mode? 
     get-channel-access-mode 
     commit-volatile-access-mode 
     checkout-volatile-access-mode 
     get-channel-access-mode-value-string
     same-string-ci?
     "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared")
    ("volatile_enable_user_level_auth" 
     valid-boolean? 
     get-boolean 
     commit-volatile-enable-user-level-auth 
     checkout-volatile-enable-user-level-auth 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("volatile_enable_per_message_auth" 
     valid-boolean? 
     get-boolean 
     commit-volatile-enable-per-message-auth 
     checkout-volatile-enable-per-message-auth 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("volatile_enable_pef_alerting" 
     valid-boolean? 
     get-boolean 
     commit-volatile-enable-pef-alerting 
     checkout-volatile-enable-pef-alerting 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("volatile_channel_privilege_limit"	
     valid-privilege-limit? 
     get-privilege-limit 
     commit-volatile-channel-privilege-limit 
     checkout-volatile-channel-privilege-limit 
     get-privilege-limit-value-string
     same-string-ci?
     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access")
    ("non_volatile_access_mode" 
     valid-channel-access-mode? 
     get-channel-access-mode 
     commit-non-volatile-access-mode 
     checkout-non-volatile-access-mode 
     get-channel-access-mode-value-string
     same-string-ci?
     "Possible values: Disabled/Pre_Boot_Only/Always_Available/Shared")
    ("non_volatile_enable_user_level_auth" 
     valid-boolean? 
     get-boolean 
     commit-non-volatile-enable-user-level-auth 
     checkout-non-volatile-enable-user-level-auth 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("non_volatile_enable_per_message_auth" 
     valid-boolean? 
     get-boolean 
     commit-non-volatile-enable-per-message-auth 
     checkout-non-volatile-enable-per-message-auth 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("non_volatile_enable_pef_alerting" 
     valid-boolean? 
     get-boolean 
     commit-non-volatile-enable-pef-alerting 
     checkout-non-volatile-enable-pef-alerting 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("non_volatile_channel_privilege_limit" 
     valid-privilege-limit? 
     get-privilege-limit 
     commit-non-volatile-channel-privilege-limit 
     checkout-non-volatile-channel-privilege-limit 
     get-privilege-limit-value-string
     same-string-ci?
     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary/No_Access")
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERTION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERTION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ))

