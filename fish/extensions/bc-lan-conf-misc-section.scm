;;; bc-lan-conf-misc-section.scm: BMC configurator LAN Conf Misc section procedures
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

(define (commit-enable-gratuitous-arps section-name enable-gratuitous-arps)
  (if (list? enable-gratuitous-arps)
      #t 
      (fi-set-bmc-lan-conf-arp-control enable-gratuitous-arps 0)))

(define (checkout-enable-gratuitous-arps section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-arp-control))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-enable-arp-response section-name enable-arp-response)
  (if (list? enable-arp-response)
      #t 
      (fi-set-bmc-lan-conf-arp-control 0 enable-arp-response)))

(define (checkout-enable-arp-response section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-arp-control))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-gratuitous-arp-interval section-name gratuitous-arp-interval)
  (if (list? gratuitous-arp-interval)
      #t 
      (fi-set-bmc-lan-conf-gratuitous-arp gratuitous-arp-interval)))

(define (checkout-gratuitous-arp-interval section-name) 
  (fi-get-bmc-lan-conf-gratuitous-arp)) 

(define lan-conf-misc-keys-validator 
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
    ("enable_gratuitous_arps" 
     valid-boolean? 
     get-boolean 
     commit-enable-gratuitous-arps 
     checkout-enable-gratuitous-arps 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("enable_arp_response" 
     valid-boolean? 
     get-boolean 
     commit-enable-arp-response 
     checkout-enable-arp-response 
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("gratuitous_arp_interval" 
     valid-integer? 
     get-integer 
     commit-gratuitous-arp-interval 
     checkout-gratuitous-arp-interval 
     any->string
     same-string-ci?
     "Give valid number. Intervals are 500 ms.")
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
