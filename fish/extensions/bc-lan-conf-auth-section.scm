;;; bc-lan-conf-auth-section.scm: BMC configurator LAN Conf Auth section procedures
;;; authors: Balamurugan <bala.a@californiadigital.com>

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA

(define (commit-callback-enable-auth-type-none section-name auth-type-none)
  (if (list? auth-type-none)
      #t 
      (fi-set-bmc-lan-conf-auth-type-callback-enables auth-type-none 0 0 0 0)))

(define (checkout-callback-enable-auth-type-none section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-callback-enables))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-callback-enable-auth-type-md2 section-name auth-type-md2)
  (if (list? auth-type-md2)
      #t 
      (fi-set-bmc-lan-conf-auth-type-callback-enables 0 auth-type-md2 0 0 0)))

(define (checkout-callback-enable-auth-type-md2 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-callback-enables))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-callback-enable-auth-type-md5 section-name auth-type-md5)
  (if (list? auth-type-md5)
      #t 
      (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 auth-type-md5 0 0)))

(define (checkout-callback-enable-auth-type-md5 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-callback-enables))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-callback-enable-auth-type-straight-password section-name auth-type-straight-password)
  (if (list? auth-type-straight-password)
      #t 
      (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 0 auth-type-straight-password 0)))

(define (checkout-callback-enable-auth-type-straight-password section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-callback-enables))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-callback-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (if (list? auth-type-oem-proprietary)
      #t 
      (fi-set-bmc-lan-conf-auth-type-callback-enables 0 0 0 0 auth-type-oem-proprietary)))

(define (checkout-callback-enable-auth-type-oem-proprietary section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-callback-enables))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define (commit-user-enable-auth-type-none section-name auth-type-none)
  (if (list? auth-type-none)
      #t 
      (fi-set-bmc-lan-conf-auth-type-user-enables auth-type-none 0 0 0 0)))

(define (checkout-user-enable-auth-type-none section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-user-enables))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-user-enable-auth-type-md2 section-name auth-type-md2)
  (if (list? auth-type-md2)
      #t 
      (fi-set-bmc-lan-conf-auth-type-user-enables 0 auth-type-md2 0 0 0)))

(define (checkout-user-enable-auth-type-md2 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-user-enables))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-user-enable-auth-type-md5 section-name auth-type-md5)
  (if (list? auth-type-md5)
      #t 
      (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 auth-type-md5 0 0)))

(define (checkout-user-enable-auth-type-md5 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-user-enables))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-user-enable-auth-type-straight-password section-name auth-type-straight-password)
  (if (list? auth-type-straight-password)
      #t 
      (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 0 auth-type-straight-password 0)))

(define (checkout-user-enable-auth-type-straight-password section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-user-enables))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-user-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (if (list? auth-type-oem-proprietary)
      #t 
      (fi-set-bmc-lan-conf-auth-type-user-enables 0 0 0 0 auth-type-oem-proprietary)))

(define (checkout-user-enable-auth-type-oem-proprietary section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-user-enables))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define (commit-operator-enable-auth-type-none section-name auth-type-none)
  (if (list? auth-type-none)
      #t 
      (fi-set-bmc-lan-conf-auth-type-operator-enables auth-type-none 0 0 0 0)))

(define (checkout-operator-enable-auth-type-none section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-operator-enables))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-operator-enable-auth-type-md2 section-name auth-type-md2)
  (if (list? auth-type-md2)
      #t 
      (fi-set-bmc-lan-conf-auth-type-operator-enables 0 auth-type-md2 0 0 0)))

(define (checkout-operator-enable-auth-type-md2 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-operator-enables))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-operator-enable-auth-type-md5 section-name auth-type-md5)
  (if (list? auth-type-md5)
      #t 
      (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 auth-type-md5 0 0)))

(define (checkout-operator-enable-auth-type-md5 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-operator-enables))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-operator-enable-auth-type-straight-password section-name auth-type-straight-password)
  (if (list? auth-type-straight-password)
      #t 
      (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 0 auth-type-straight-password 0)))

(define (checkout-operator-enable-auth-type-straight-password section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-operator-enables))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-operator-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (if (list? auth-type-oem-proprietary)
      #t 
      (fi-set-bmc-lan-conf-auth-type-operator-enables 0 0 0 0 auth-type-oem-proprietary)))

(define (checkout-operator-enable-auth-type-oem-proprietary section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-operator-enables))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define (commit-admin-enable-auth-type-none section-name auth-type-none)
  (if (list? auth-type-none)
      #t 
      (fi-set-bmc-lan-conf-auth-type-admin-enables auth-type-none 0 0 0 0)))

(define (checkout-admin-enable-auth-type-none section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-admin-enables))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-admin-enable-auth-type-md2 section-name auth-type-md2)
  (if (list? auth-type-md2)
      #t 
      (fi-set-bmc-lan-conf-auth-type-admin-enables 0 auth-type-md2 0 0 0)))

(define (checkout-admin-enable-auth-type-md2 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-admin-enables))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-admin-enable-auth-type-md5 section-name auth-type-md5)
  (if (list? auth-type-md5)
      #t 
      (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 auth-type-md5 0 0)))

(define (checkout-admin-enable-auth-type-md5 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-admin-enables))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-admin-enable-auth-type-straight-password section-name auth-type-straight-password)
  (if (list? auth-type-straight-password)
      #t 
      (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 0 auth-type-straight-password 0)))

(define (checkout-admin-enable-auth-type-straight-password section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-admin-enables))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-admin-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (if (list? auth-type-oem-proprietary)
      #t 
      (fi-set-bmc-lan-conf-auth-type-admin-enables 0 0 0 0 auth-type-oem-proprietary)))

(define (checkout-admin-enable-auth-type-oem-proprietary section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-admin-enables))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define (commit-oem-enable-auth-type-none section-name auth-type-none)
  (if (list? auth-type-none)
      #t 
      (fi-set-bmc-lan-conf-auth-type-oem-enables auth-type-none 0 0 0 0)))

(define (checkout-oem-enable-auth-type-none section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-oem-enables))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-oem-enable-auth-type-md2 section-name auth-type-md2)
  (if (list? auth-type-md2)
      #t 
      (fi-set-bmc-lan-conf-auth-type-oem-enables 0 auth-type-md2 0 0 0)))

(define (checkout-oem-enable-auth-type-md2 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-oem-enables))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-oem-enable-auth-type-md5 section-name auth-type-md5)
  (if (list? auth-type-md5)
      #t 
      (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 auth-type-md5 0 0)))

(define (checkout-oem-enable-auth-type-md5 section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-oem-enables))) 
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-oem-enable-auth-type-straight-password section-name auth-type-straight-password)
  (if (list? auth-type-straight-password)
      #t 
      (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 0 auth-type-straight-password 0)))

(define (checkout-oem-enable-auth-type-straight-password section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-oem-enables))) 
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-oem-enable-auth-type-oem-proprietary section-name auth-type-oem-proprietary)
  (if (list? auth-type-oem-proprietary)
      #t 
      (fi-set-bmc-lan-conf-auth-type-oem-enables 0 0 0 0 auth-type-oem-proprietary)))

(define (checkout-oem-enable-auth-type-oem-proprietary section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-auth-type-oem-enables))) 
    (if (list? param-list) (cddddr param-list) #f)))

(define lan-conf-auth-keys-validator 
  '(
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERSION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERSION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ("callback_enable_auth_type_none" 
     valid-boolean? 
     get-boolean 
     commit-callback-enable-auth-type-none 
     checkout-callback-enable-auth-type-none 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No")
    ("callback_enable_auth_type_md2" 
     valid-boolean? 
     get-boolean 
     commit-callback-enable-auth-type-md2 
     checkout-callback-enable-auth-type-md2 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("callback_enable_auth_type_md5" 
     valid-boolean? 
     get-boolean 
     commit-callback-enable-auth-type-md5 
     checkout-callback-enable-auth-type-md5 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("callback_enable_auth_type_straight_password" 
     valid-boolean? 
     get-boolean 
     commit-callback-enable-auth-type-straight-password 
     checkout-callback-enable-auth-type-straight-password 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("callback_enable_auth_type_oem_proprietary" 
     valid-boolean? 
     get-boolean 
     commit-callback-enable-auth-type-oem-proprietary 
     checkout-callback-enable-auth-type-oem-proprietary 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("user_enable_auth_type_none" 
     valid-boolean? 
     get-boolean 
     commit-user-enable-auth-type-none 
     checkout-user-enable-auth-type-none 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("user_enable_auth_type_md2" 
     valid-boolean? 
     get-boolean 
     commit-user-enable-auth-type-md2 
     checkout-user-enable-auth-type-md2 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("user_enable_auth_type_md5" 
     valid-boolean? 
     get-boolean 
     commit-user-enable-auth-type-md5 
     checkout-user-enable-auth-type-md5 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("user_enable_auth_type_straight_password" 
     valid-boolean? 
     get-boolean 
     commit-user-enable-auth-type-straight-password 
     checkout-user-enable-auth-type-straight-password 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("user_enable_auth_type_oem_proprietary" 
     valid-boolean? 
     get-boolean 
     commit-user-enable-auth-type-oem-proprietary 
     checkout-user-enable-auth-type-oem-proprietary 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("operator_enable_auth_type_none" 
     valid-boolean? 
     get-boolean 
     commit-operator-enable-auth-type-none 
     checkout-operator-enable-auth-type-none 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("operator_enable_auth_type_md2" 
     valid-boolean? 
     get-boolean 
     commit-operator-enable-auth-type-md2 
     checkout-operator-enable-auth-type-md2 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("operator_enable_auth_type_md5" 
     valid-boolean? 
     get-boolean 
     commit-operator-enable-auth-type-md5 
     checkout-operator-enable-auth-type-md5 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("operator_enable_auth_type_straight_password" 
     valid-boolean? 
     get-boolean 
     commit-operator-enable-auth-type-straight-password 
     checkout-operator-enable-auth-type-straight-password 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("operator_enable_auth_type_oem_proprietary" 
     valid-boolean? 
     get-boolean 
     commit-operator-enable-auth-type-oem-proprietary 
     checkout-operator-enable-auth-type-oem-proprietary 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("admin_enable_auth_type_none" 
     valid-boolean? 
     get-boolean 
     commit-admin-enable-auth-type-none 
     checkout-admin-enable-auth-type-none 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("admin_enable_auth_type_md2" 
     valid-boolean? 
     get-boolean 
     commit-admin-enable-auth-type-md2 
     checkout-admin-enable-auth-type-md2 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("admin_enable_auth_type_md5" 
     valid-boolean? 
     get-boolean 
     commit-admin-enable-auth-type-md5 
     checkout-admin-enable-auth-type-md5 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("admin_enable_auth_type_straight_password" 
     valid-boolean? 
     get-boolean 
     commit-admin-enable-auth-type-straight-password 
     checkout-admin-enable-auth-type-straight-password 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("admin_enable_auth_type_oem_proprietary" 
     valid-boolean? 
     get-boolean 
     commit-admin-enable-auth-type-oem-proprietary 
     checkout-admin-enable-auth-type-oem-proprietary 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("oem_enable_auth_type_none" 
     valid-boolean? 
     get-boolean 
     commit-oem-enable-auth-type-none 
     checkout-oem-enable-auth-type-none 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("oem_enable_auth_type_md2" 
     valid-boolean? 
     get-boolean 
     commit-oem-enable-auth-type-md2 
     checkout-oem-enable-auth-type-md2 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("oem_enable_auth_type_md5" 
     valid-boolean? 
     get-boolean 
     commit-oem-enable-auth-type-md5 
     checkout-oem-enable-auth-type-md5 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("oem_enable_auth_type_straight_password" 
     valid-boolean? 
     get-boolean 
     commit-oem-enable-auth-type-straight-password 
     checkout-oem-enable-auth-type-straight-password 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ("oem_enable_auth_type_oem_proprietary" 
     valid-boolean? 
     get-boolean 
     commit-oem-enable-auth-type-oem-proprietary 
     checkout-oem-enable-auth-type-oem-proprietary 
     get-boolean-string
     same-string-ci? 
     "Possible values: Yes/No")
    ;; You can add more in the form of 
    ;; (KEYSTRING 
    ;;  VALIDATION-PROC 
    ;;  CONVERSION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERSION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
))
