;;; bc-lan-conf-security-keys-section.scm: BMC configurator LAN Conf Security-Keys section procedures
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

(define (commit-k-r section-name k-r)
  (if (list? k-r)
      (set! k-r ""))
  (fi-set-k-r k-r))

(define (checkout-k-r section-name)
  (fi-get-k-r))

(define (commit-k-g section-name k-g)
  (if (list? k-g)
      (set! k-g ""))
  (fi-set-k-g k-g))

(define (checkout-k-g section-name)
  (fi-get-k-g))

(define lan-conf-security-keys-validator 
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
    ("k_r" 
     valid-security-key?
     get-string
     commit-k-r
     checkout-k-r
     get-string
     same-string-ci?
     "Hex String")
    ("k_g" 
     valid-security-key?
     get-string
     commit-k-g
     checkout-k-g
     get-string
     same-string-ci?
     "Hex String")
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
