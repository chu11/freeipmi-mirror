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
    ;;  CONVERTION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERTION-PROC 
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
    ;;  CONVERTION-PROC 
    ;;  BMC-COMMIT-PROC 
    ;;  BMC-CHECKOUT-PROC 
    ;;  VALUE-CONVERTION-PROC 
    ;;  DIFF-PROC 
    ;;  DOC-STRING)
    ))
