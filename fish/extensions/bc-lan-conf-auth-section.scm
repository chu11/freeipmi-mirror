(define (commit-callback-enable-auth-type-none section-name auth-type-none)
  (fi-set-bmc-lan-conf-auth-type-callback-enables auth-type-none 0 0 0 0))

(define (commit-callback-enable-auth-type-md2 section-name auth-type-md2)
  (fi-set-bmc-lan-conf-auth-type-callback-enables 0 auth-type-md2 0 0 0))

(define (commit-callback-enable-auth-type-md5 section-name auth-type-md5)
  (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 auth-type-md5 0 0))

(define (commit-callback-enable-auth-type-straight-password section-name auth-type-straight-password)
  (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 0 auth-type-straight-password 0))

(define (commit-callback-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 0 0 auth-type-oem-proprietary))

(define (commit-user-enable-auth-type-none section-name auth-type-none)
  (fi-set-bmc-lan-conf-auth-type-user-enables auth-type-none 0 0 0 0))

(define (commit-user-enable-auth-type-md2 section-name auth-type-md2)
  (fi-set-bmc-lan-conf-auth-type-user-enables 0 auth-type-md2 0 0 0))

(define (commit-user-enable-auth-type-md5 section-name auth-type-md5)
  (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 auth-type-md5 0 0))

(define (commit-user-enable-auth-type-straight-password section-name auth-type-straight-password)
  (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 0 auth-type-straight-password 0))

(define (commit-user-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 0 0 auth-type-oem-proprietary))

(define (commit-operator-enable-auth-type-none section-name auth-type-none)
  (fi-set-bmc-lan-conf-auth-type-operator-enables auth-type-none 0 0 0 0))

(define (commit-operator-enable-auth-type-md2 section-name auth-type-md2)
  (fi-set-bmc-lan-conf-auth-type-operator-enables 0 auth-type-md2 0 0 0))

(define (commit-operator-enable-auth-type-md5 section-name auth-type-md5)
  (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 auth-type-md5 0 0))

(define (commit-operator-enable-auth-type-straight-password section-name auth-type-straight-password)
  (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 0 auth-type-straight-password 0))

(define (commit-operator-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 0 0 auth-type-oem-proprietary))

(define (commit-admin-enable-auth-type-none section-name auth-type-none)
  (fi-set-bmc-lan-conf-auth-type-admin-enables auth-type-none 0 0 0 0))

(define (commit-admin-enable-auth-type-md2 section-name auth-type-md2)
  (fi-set-bmc-lan-conf-auth-type-admin-enables 0 auth-type-md2 0 0 0))

(define (commit-admin-enable-auth-type-md5 section-name auth-type-md5)
  (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 auth-type-md5 0 0))

(define (commit-admin-enable-auth-type-straight-password section-name auth-type-straight-password)
  (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 0 auth-type-straight-password 0))

(define (commit-admin-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 0 0 auth-type-oem-proprietary))

(define (commit-oem-enable-auth-type-none section-name auth-type-none)
  (fi-set-bmc-lan-conf-auth-type-oem-enables auth-type-none 0 0 0 0))

(define (commit-oem-enable-auth-type-md2 section-name auth-type-md2)
  (fi-set-bmc-lan-conf-auth-type-oem-enables 0 auth-type-md2 0 0 0))

(define (commit-oem-enable-auth-type-md5 section-name auth-type-md5)
  (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 auth-type-md5 0 0))

(define (commit-oem-enable-auth-type-straight-password section-name auth-type-straight-password)
  (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 0 auth-type-straight-password 0))

(define (commit-oem-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 0 0 auth-type-oem-proprietary))

(define lan-conf-auth-keys-validator 
  '(("callback_enable_auth_type_none" valid-boolean? get-boolean commit-callback-enable-auth-type-none)
    ("callback_enable_auth_type_md2" valid-boolean? get-boolean commit-callback-enable-auth-type-md2)
    ("callback_enable_auth_type_md5" valid-boolean? get-boolean commit-callback-enable-auth-type-md5)
    ("callback_enable_auth_type_straight_password" valid-boolean? get-boolean commit-callback-enable-auth-type-straight-password)
    ("callback_enable_auth_type_oem_proprietary" valid-boolean? get-boolean commit-callback-enable-auth-type-oem-proprietary)
    ("user_enable_auth_type_none" valid-boolean? get-boolean commit-user-enable-auth-type-none)
    ("user_enable_auth_type_md2" valid-boolean? get-boolean commit-user-enable-auth-type-md2)
    ("user_enable_auth_type_md5" valid-boolean? get-boolean commit-user-enable-auth-type-md5)
    ("user_enable_auth_type_straight_password" valid-boolean? get-boolean commit-user-enable-auth-type-straight-password)
    ("user_enable_auth_type_oem_proprietary" valid-boolean? get-boolean commit-user-enable-auth-type-oem-proprietary)
    ("operator_enable_auth_type_none" valid-boolean? get-boolean commit-operator-enable-auth-type-none)
    ("operator_enable_auth_type_md2" valid-boolean? get-boolean commit-operator-enable-auth-type-md2)
    ("operator_enable_auth_type_md5" valid-boolean? get-boolean commit-operator-enable-auth-type-md5)
    ("operator_enable_auth_type_straight_password" valid-boolean? get-boolean commit-operator-enable-auth-type-straight-password)
    ("operator_enable_auth_type_oem_proprietary" valid-boolean? get-boolean commit-operator-enable-auth-type-oem-proprietary)
    ("admin_enable_auth_type_none" valid-boolean? get-boolean commit-admin-enable-auth-type-none)
    ("admin_enable_auth_type_md2" valid-boolean? get-boolean commit-admin-enable-auth-type-md2)
    ("admin_enable_auth_type_md5" valid-boolean? get-boolean commit-admin-enable-auth-type-md5)
    ("admin_enable_auth_type_straight_password" valid-boolean? get-boolean commit-admin-enable-auth-type-straight-password)
    ("admin_enable_auth_type_oem_proprietary" valid-boolean? get-boolean commit-admin-enable-auth-type-oem-proprietary)
    ("oem_enable_auth_type_none" valid-boolean? get-boolean commit-oem-enable-auth-type-none)
    ("oem_enable_auth_type_md2" valid-boolean? get-boolean commit-oem-enable-auth-type-md2)
    ("oem_enable_auth_type_md5" valid-boolean? get-boolean commit-oem-enable-auth-type-md5)
    ("oem_enable_auth_type_straight_password" valid-boolean? get-boolean commit-oem-enable-auth-type-straight-password)
    ("oem_enable_auth_type_oem_proprietary" valid-boolean? get-boolean commit-oem-enable-auth-type-oem-proprietary)
    ;; You can add more by
    ;; (KEY-STRING  VALUE-VALIDATOR-PROC  VALUE-CONVERTOR-PROC  VALUE-COMMIT-PROC)
))
