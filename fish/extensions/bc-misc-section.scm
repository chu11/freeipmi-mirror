(define (commit-power-restore-policy section-name power-restore-policy)
  (fi-set-bmc-power-restore-policy power-restore-policy))

(define (checkout-power-restore-policy section-name) 
  (fi-get-bmc-power-restore-policy)) 

(define misc-keys-validator 
  '(("power_restore_policy" 
     valid-power-restore-policy? 
     get-power-restore-policy 
     commit-power-restore-policy 
     checkout-power-restore-policy 
     get-power-restore-policy-value-string
     "Possible Values: Off_State_AC_Apply/Restore_State_AC_Apply/On_State_AC_Apply")
    ))
