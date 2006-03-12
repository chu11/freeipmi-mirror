;;; bc-rmcpplus-conf-privilege-section.scm: BMC configurator for RMCPPLUS privileges
;;; authors: Al Chu <chu11@llnl.gov>

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

(define (commit-cipher-id-0 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-0 priv-type)))

(define (checkout-cipher-id-0 section-name)
  (fi-get-rmcpplus-cipher-suite-id-0))

(define (commit-cipher-id-1 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-1 priv-type)))

(define (checkout-cipher-id-1 section-name)
  (fi-get-rmcpplus-cipher-suite-id-1))

(define (commit-cipher-id-2 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-2 priv-type)))

(define (checkout-cipher-id-2 section-name)
  (fi-get-rmcpplus-cipher-suite-id-2))

(define (commit-cipher-id-3 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-3 priv-type)))

(define (checkout-cipher-id-3 section-name)
  (fi-get-rmcpplus-cipher-suite-id-3))

(define (commit-cipher-id-4 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-4 priv-type)))

(define (checkout-cipher-id-4 section-name)
  (fi-get-rmcpplus-cipher-suite-id-4))

(define (commit-cipher-id-5 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-5 priv-type)))

(define (checkout-cipher-id-5 section-name)
  (fi-get-rmcpplus-cipher-suite-id-5))

(define (commit-cipher-id-6 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-6 priv-type)))

(define (checkout-cipher-id-6 section-name)
  (fi-get-rmcpplus-cipher-suite-id-6))

(define (commit-cipher-id-7 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-7 priv-type)))

(define (checkout-cipher-id-7 section-name)
  (fi-get-rmcpplus-cipher-suite-id-7))

(define (commit-cipher-id-8 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-8 priv-type)))

(define (checkout-cipher-id-8 section-name)
  (fi-get-rmcpplus-cipher-suite-id-8))

(define (commit-cipher-id-9 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-9 priv-type)))

(define (checkout-cipher-id-9 section-name)
  (fi-get-rmcpplus-cipher-suite-id-9))

(define (commit-cipher-id-10 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-10 priv-type)))

(define (checkout-cipher-id-10 section-name)
  (fi-get-rmcpplus-cipher-suite-id-10))

(define (commit-cipher-id-11 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-11 priv-type)))

(define (checkout-cipher-id-11 section-name)
  (fi-get-rmcpplus-cipher-suite-id-11))

(define (commit-cipher-id-12 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-12 priv-type)))

(define (checkout-cipher-id-12 section-name)
  (fi-get-rmcpplus-cipher-suite-id-12))

(define (commit-cipher-id-13 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-13 priv-type)))

(define (checkout-cipher-id-13 section-name)
  (fi-get-rmcpplus-cipher-suite-id-13))

(define (commit-cipher-id-14 section-name priv-type)
  (if (list? priv-type)
      #t 
      (fi-set-rmcpplus-cipher-suite-id-14 priv-type)))

(define (checkout-cipher-id-14 section-name)
  (fi-get-rmcpplus-cipher-suite-id-14))

(define rmcpplus-conf-privilege-keys-validator
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
    ("Maximum_Privilege_Cipher_Suite_Id_0" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-0
     checkout-cipher-id-0
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_1" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-1
     checkout-cipher-id-1
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_2" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-2
     checkout-cipher-id-2
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_3" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-3
     checkout-cipher-id-3
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_4" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-4
     checkout-cipher-id-4
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_5" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-5
     checkout-cipher-id-5
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_6" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-6
     checkout-cipher-id-6
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_7" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-7
     checkout-cipher-id-7
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_8" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-8
     checkout-cipher-id-8
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_9" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-9
     checkout-cipher-id-9
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_10" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-10
     checkout-cipher-id-10
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_11" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-11
     checkout-cipher-id-11
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_12" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-12
     checkout-cipher-id-12
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_13" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-13
     checkout-cipher-id-13
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
    ("Maximum_Privilege_Cipher_Suite_Id_14" 
     valid-rmcpplus-max-privilege?
     get-rmcpplus-max-privilege
     commit-cipher-id-14
     checkout-cipher-id-14
     get-rmcpplus-max-privilege-value-string
     same-string-ci? 
     "Possible values: Unused/User/Operator/Administrator/OEM_Proprietary")
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
