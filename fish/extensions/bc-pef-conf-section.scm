;;; bc-pef-conf-section.scm: BMC configurator PEF Conf section procedures
;;; author: A Balamurugan <bala@zresearch.com>

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

(define (commit-enable-pef section-name enable-pef)
  (if (list? enable-pef)
      #t 
      (fi-set-bmc-pef-conf-pef-control enable-pef 0 0 0)))

(define (checkout-enable-pef section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-control)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-enable-pef-event-messages section-name 
					  enable-pef-event-message)
  (if (list? enable-pef-event-message)
      #t 
      (fi-set-bmc-pef-conf-pef-control 0 enable-pef-event-message 0 0)))

(define (checkout-enable-pef-event-messages section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-control)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-enable-pef-startup-delay section-name 
					 enable-pef-startup-delay)
  (if (list? enable-pef-startup-delay)
      #t 
      (fi-set-bmc-pef-conf-pef-control 0 0 enable-pef-startup-delay 0)))

(define (checkout-enable-pef-startup-delay section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-control)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-enable-pef-alert-startup-delay section-name 
					       enable-pef-alert-startup-delay)
  (if (list? enable-pef-alert-startup-delay)
      #t 
      (fi-set-bmc-pef-conf-pef-control 0 0 0 enable-pef-alert-startup-delay)))

(define (checkout-enable-pef-alert-startup-delay section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-control)))
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-enable-alert-action section-name 
				    enable-alert-action)
  (if (list? enable-alert-action)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control enable-alert-action 0 0 0 0 0)))

(define (checkout-enable-alert-action section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-enable-powerdown-action section-name 
					enable-powerdowm-action)
  (if (list? enable-powerdown-action)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control 0 enable-powerdowm-action 0 0 0 0)))

(define (checkout-enable-powerdown-action section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-enable-reset-action section-name 
				    enable-reset-action)
  (if (list? enable-reset-action)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control 0 0 enable-reset-action 0 0 0)))

(define (checkout-enable-reset-action section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (caddr param-list)) #f)))

(define (commit-enable-powercycle-action section-name 
					 enable-powercycle-action)
  (if (list? enable-powercycle-action)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control 0 0 0 enable-powercycle-action 0 0)))

(define (checkout-enable-powercycle-action section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (cadddr param-list)) #f)))

(define (commit-enable-oem-action section-name 
				  enable-oem-action)
  (if (list? enable-oem-action)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control 0 0 0 0 enable-oem-action 0)))

(define (checkout-enable-oem-action section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (caddddr param-list)) #f)))

(define (commit-enable-diag-interrupt section-name 
				      enable-diag-interrupt)
  (if (list? enable-diag-interrupt)
      #t 
      (fi-set-bmc-pef-conf-pef-global-action-control 0 0 0 0 0 enable-diag-interrupt)))

(define (checkout-enable-diag-interrupt section-name)
  (let ((param-list (fi-get-bmc-pef-conf-pef-global-action-control)))
    (if (list? param-list) (list (cadddddr param-list)) #f)))

(define (commit-pef-startup-delay section-name 
				  pef-startup-delay)
  (if (list? pef-startup-delay)
      #t 
      (fi-set-bmc-pef-conf-pef-startup-delay pef-startup-delay)))

(define (checkout-pef-startup-delay section-name)
  (fi-get-bmc-pef-conf-pef-startup-delay))

(define (commit-pef-alert-startup-delay section-name 
					pef-alert-startup-delay)
  (if (list? pef-alert-startup-delay)
      #t 
      (fi-set-bmc-pef-conf-pef-alert-startup-delay pef-alert-startup-delay)))

(define (checkout-pef-alert-startup-delay section-name)
  (fi-get-bmc-pef-conf-pef-alert-startup-delay))

(define pef-conf-keys-validator 
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
    ("Enable_PEF" 
     valid-boolean? 
     get-boolean 
     commit-enable-pef 
     checkout-enable-pef 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_PEF_Event_Messages" 
     valid-boolean? 
     get-boolean 
     commit-enable-pef-event-messages 
     checkout-enable-pef-event-messages 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_PEF_Startup_Delay" 
     valid-boolean? 
     get-boolean 
     commit-enable-pef-startup-delay 
     checkout-enable-pef-startup-delay 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_PEF_Alert_Startup_Delay" 
     valid-boolean? 
     get-boolean 
     commit-enable-pef-alert-startup-delay 
     checkout-enable-pef-alert-startup-delay 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_Alert_Action" 
     valid-boolean? 
     get-boolean 
     commit-enable-alert-action 
     checkout-enable-alert-action 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_Powerdowm_Action" 
     valid-boolean? 
     get-boolean 
     commit-enable-powerdown-action 
     checkout-enable-powerdown-action 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_Reset_Action" 
     valid-boolean? 
     get-boolean 
     commit-enable-reset-action 
     checkout-enable-reset-action 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_Powercycle_Action" 
     valid-boolean? 
     get-boolean 
     commit-enable-powercycle-action 
     checkout-enable-powercycle-action 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_OEM_Action" 
     valid-boolean? 
     get-boolean 
     commit-enable-oem-action 
     checkout-enable-oem-action 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("Enable_Diagnostic_Interrupt" 
     valid-boolean? 
     get-boolean 
     commit-enable-diag-interrupt 
     checkout-enable-diag-interrupt 
     get-boolean-string 
     same-string-ci? 
     "Possible values: Yes/No") 
    ("PEF_Startup_Delay" 
     valid-integer? 
     get-integer 
     commit-pef-startup-delay 
     checkout-pef-startup-delay 
     any->string 
     same-string-ci? 
     "Give value in seconds") 
    ("PEF_Alert_Startup_Delay" 
     valid-integer? 
     get-integer 
     commit-pef-alert-startup-delay 
     checkout-pef-alert-startup-delay 
     any->string 
     same-string-ci? 
     "Give value in seconds") 
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

