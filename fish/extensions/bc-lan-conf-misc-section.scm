(define (commit-enable-gratuitous-arps section-name enable-gratuitous-arps)
  (fi-set-bmc-lan-conf-arp-control enable-gratuitous-arps 0))

(define (checkout-enable-gratuitous-arps section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-arp-control))) 
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-enable-arp-response section-name enable-arp-response)
  (fi-set-bmc-lan-conf-arp-control 0 enable-arp-response))

(define (checkout-enable-arp-response section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-arp-control))) 
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-gratuitous-arp-interval section-name gratuitous-arp-interval)
  (fi-set-bmc-lan-conf-gratuitous-arp gratuitous-arp-interval))

(define (checkout-gratuitous-arp-interval section-name) 
  (fi-get-bmc-lan-conf-gratuitous-arp)) 

(define lan-conf-misc-keys-validator 
  '(("enable_gratuitous_arps" 
     valid-boolean? 
     get-boolean 
     commit-enable-gratuitous-arps 
     checkout-enable-gratuitous-arps 
     get-boolean-string
     "Possible values: Yes/No")
    ("enable_arp_response" 
     valid-boolean? 
     get-boolean 
     commit-enable-arp-response 
     checkout-enable-arp-response 
     get-boolean-string
     "Possible values: Yes/No")
    ("gratuitous_arp_interval" 
     valid-integer? 
     get-integer 
     commit-gratuitous-arp-interval 
     checkout-gratuitous-arp-interval 
     simple->string
     "Give valid number")
))
