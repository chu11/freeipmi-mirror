;;; bc-sol-conf-section.scm: BMC configurator Sol Conf
;;;                             section procedures
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

(define (commit-enable-sol section-name enable-sol)
  (if (list? enable-sol)
      #t 
      (fi-set-sol-sol-enable enable-sol)))

(define (checkout-enable-sol section-name) 
  (fi-get-sol-sol-enable)) 

(define (commit-sol-privilege-level section-name sol-privilege-level)
  (if (list? sol-privilege-level)
      #t 
      (fi-set-sol-sol-authentication sol-privilege-level 0 0)))

(define (checkout-sol-privilege-level section-name)
  (let ((param-list (fi-get-sol-sol-authentication)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-force-sol-payload-authentication section-name force-sol-payload-authentication)
  (if (list? force-sol-payload-authentication)
      #t 
      (fi-set-sol-sol-authentication 0 force-sol-payload-authentication 0)))

(define (checkout-force-sol-payload-authentication section-name)
  (let ((param-list (fi-get-sol-sol-authentication)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-force-sol-payload-encryption section-name force-sol-payload-encryption)
  (if (list? force-sol-payload-encryption)
      #t 
      (fi-set-sol-sol-authentication 0 0 force-sol-payload-encryption)))

(define (checkout-force-sol-payload-encryption section-name)
  (let ((param-list (fi-get-sol-sol-encryption)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-character-accumulate-interval section-name character-accumulate-interval)
  (if (list? character-accumulate-interval)
      #t 
      (fi-set-sol-character-accumulate-interval-and-send-threshold character-accumulate-interval 0)))

(define (checkout-character-accumulate-interval section-name)
  (let ((param-list (fi-get-sol-character-accumulate-interval-and-send-threshold)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-character-send-threshold section-name character-send-threshold)
  (if (list? character-send-threshold)
      #t 
      (fi-set-sol-character-accumulate-interval-and-send-threshold 0 character-send-threshold)))

(define (checkout-character-send-threshold section-name)
  (let ((param-list (fi-get-sol-sol-encryption)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-sol-retry-count section-name sol-retry-count)
  (if (list? sol-retry-count)
      #t 
      (fi-set-sol-sol-retry sol-retry-count 0)))

(define (checkout-sol-retry-count section-name)
  (let ((param-list (fi-get-sol-sol-retry)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-sol-retry-interval section-name sol-retry-interval)
  (if (list? sol-retry-interval)
      #t 
      (fi-set-sol-sol-retry 0 sol-retry-interval)))

(define (checkout-sol-retry-interval section-name)
  (let ((param-list (fi-get-sol-sol-encryption)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-non-volatile-bit-rate section-name non-volatile-bit-rate)
  (if (list? non-volatile-bit-rate)
      #t 
      (fi-set-sol-sol-non-volatile-bit-rate non-volatile-bit-rate)))

(define (checkout-non-volatile-bit-rate section-name) 
  (fi-get-sol-sol-non-volatile-bit-rate)) 

(define (commit-volatile-bit-rate section-name volatile-bit-rate)
  (if (list? volatile-bit-rate)
      #t 
      (fi-set-sol-sol-volatile-bit-rate volatile-bit-rate)))

(define (checkout-volatile-bit-rate section-name) 
  (fi-get-sol-sol-volatile-bit-rate)) 

(define (commit-payload-port-number section-name payload-port-number)
  (if (list? payload-port-number)
      #t 
      (fi-set-sol-sol-payload-port-number payload-port-number)))

(define (checkout-payload-port-number section-name) 
  (fi-get-sol-sol-payload-port-number)) 

(define sol-conf-keys-validator 
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
    ("enable_sol" 
     valid-boolean? 
     get-boolean 
     commit-enable-sol
     checkout-enable-sol 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("sol_privilege_level" 
     valid-privilege-level?
     get-privilege-level 
     commit-sol-privilege-level 
     checkout-sol-privilege-level 
     get-privilege-level-value-string
     same-string-ci?
     "Possible values: Callback/User/Operator/Administrator/OEM_Proprietary")
    ("Force_SOL_Payload_Authentication" 
     valid-boolean? 
     get-boolean 
     commit-force-sol-payload-authentication
     checkout-force-sol-payload-authentication 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("Force_SOL_Payload_Encryption" 
     valid-boolean? 
     get-boolean 
     commit-force-sol-payload-encryption
     checkout-force-sol-payload-encryption 
     get-boolean-string 
     same-string-ci?
     "Possible values: Yes/No")
    ("character_accumulate_interval" 
     valid-integer? 
     get-integer 
     commit-character-accumulate-interval 
     checkout-character-accumulate-interval 
     any->string
     same-string-ci?
     "Give valid number.  Intervals are 5 ms.")
    ("character_send_threshold" 
     valid-integer? 
     get-integer 
     commit-character-send-threshold 
     checkout-character-send-threshold 
     any->string
     same-string-ci?
     "Give valid number")
    ("sol_retry_count" 
     valid-integer? 
     get-integer 
     commit-sol-retry-count 
     checkout-sol-retry-count 
     any->string
     same-string-ci?
     "Give valid number")
    ("sol_retry_interval" 
     valid-integer? 
     get-integer 
     commit-sol-retry-interval 
     checkout-sol-retry-interval 
     any->string
     same-string-ci?
     "Give valid number")
    ("non_volatile_bit_rate" 
     valid-sol-bit-rate? 
     get-sol-bit-rate 
     commit-non-volatile-bit-rate 
     checkout-non-volatile-bit-rate 
     get-sol-bit-rate-value-string
     same-string-ci?
     "Possible values: 9600/19200/38400/57600/115200")
    ("volatile_bit_rate" 
     valid-flow-control? 
     get-flow-control 
     commit-volatile-bit-rate 
     checkout-volatile-bit-rate 
     get-sol-bit-rate-value-string
     same-string-ci?
     "Possible values: 9600/19200/38400/57600/115200")
    ("sol_payload_port_number" 
     valid-integer? 
     get-integer 
     commit-payload-port-number 
     checkout-payload-port-number 
     any->string
     same-string-ci?
     "Give valid number")
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
