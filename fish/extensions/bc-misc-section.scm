;;; bc-misc-section.scm: BMC configurator Misc
;;;                      section procedures
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

(define (commit-power-restore-policy section-name power-restore-policy)
  (if (list? power-restore-policy)
      #t 
      (fi-set-bmc-power-restore-policy power-restore-policy)))

(define (checkout-power-restore-policy section-name) 
  (fi-get-bmc-power-restore-policy)) 

(define misc-keys-validator 
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
    ("power_restore_policy" 
     valid-power-restore-policy? 
     get-power-restore-policy 
     commit-power-restore-policy 
     checkout-power-restore-policy 
     get-power-restore-policy-value-string
     same-string-ci?
     "Possible Values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply")
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
