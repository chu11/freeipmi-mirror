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
  '(("Callback_Enable_Auth_Type_None" valid-boolean? get-boolean commit-callback-enable-auth-type-none)
    ("Callback_Enable_Auth_Type_MD2" valid-boolean? get-boolean commit-callback-enable-auth-type-md2)
    ("Callback_Enable_Auth_Type_MD5" valid-boolean? get-boolean commit-callback-enable-auth-type-md5)
    ("Callback_Enable_Auth_Type_Straight_Password" valid-boolean? get-boolean commit-callback-enable-auth-type-straight-password)
    ("Callback_Enable_Auth_Type_OEM_Proprietary" valid-boolean? get-boolean commit-callback-enable-auth-type-oem-proprietary)
    ("User_Enable_Auth_Type_None" valid-boolean? get-boolean commit-user-enable-auth-type-none)
    ("User_Enable_Auth_Type_MD2" valid-boolean? get-boolean commit-user-enable-auth-type-md2)
    ("User_Enable_Auth_Type_MD5" valid-boolean? get-boolean commit-user-enable-auth-type-md5)
    ("User_Enable_Auth_Type_Straight_Password" valid-boolean? get-boolean commit-user-enable-auth-type-straight-password)
    ("User_Enable_Auth_Type_OEM_Proprietary" valid-boolean? get-boolean commit-user-enable-auth-type-oem-proprietary)
    ("Operator_Enable_Auth_Type_None" valid-boolean? get-boolean commit-operator-enable-auth-type-none)
    ("Operator_Enable_Auth_Type_MD2" valid-boolean? get-boolean commit-operator-enable-auth-type-md2)
    ("Operator_Enable_Auth_Type_MD5" valid-boolean? get-boolean commit-operator-enable-auth-type-md5)
    ("Operator_Enable_Auth_Type_Straight_Password" valid-boolean? get-boolean commit-operator-enable-auth-type-straight-password)
    ("Operator_Enable_Auth_Type_OEM_Proprietary" valid-boolean? get-boolean commit-operator-enable-auth-type-oem-proprietary)
    ("Admin_Enable_Auth_Type_None" valid-boolean? get-boolean commit-admin-enable-auth-type-none)
    ("Admin_Enable_Auth_Type_MD2" valid-boolean? get-boolean commit-admin-enable-auth-type-md2)
    ("Admin_Enable_Auth_Type_MD5" valid-boolean? get-boolean commit-admin-enable-auth-type-md5)
    ("Admin_Enable_Auth_Type_Straight_Password" valid-boolean? get-boolean commit-admin-enable-auth-type-straight-password)
    ("Admin_Enable_Auth_Type_OEM_Proprietary" valid-boolean? get-boolean commit-admin-enable-auth-type-oem-proprietary)
    ("Oem_Enable_Auth_Type_None" valid-boolean? get-boolean commit-oem-enable-auth-type-none)
    ("Oem_Enable_Auth_Type_MD2" valid-boolean? get-boolean commit-oem-enable-auth-type-md2)
    ("Oem_Enable_Auth_Type_MD5" valid-boolean? get-boolean commit-oem-enable-auth-type-md5)
    ("Oem_Enable_Auth_Type_Straight_Password" valid-boolean? get-boolean commit-oem-enable-auth-type-straight-password)
    ("Oem_Enable_Auth_Type_OEM_Proprietary" valid-boolean? get-boolean commit-oem-enable-auth-type-oem-proprietary)))
