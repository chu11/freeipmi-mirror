(define (commit-ip-address-source section-name ip-address-source)
  (fi-set-bmc-lan-conf-ip-address-source ip-address-source))

(define (checkout-ip-address-source section-name) 
  (fi-get-bmc-lan-conf-ip-address-source)) 

(define (commit-ip-address section-name ip-address)
  (fi-set-bmc-lan-conf-ip-address ip-address))

(define (checkout-ip-address section-name) 
  (fi-get-bmc-lan-conf-ip-address)) 

(define (commit-mac-address section-name mac-address)
  (fi-set-bmc-lan-conf-mac-address mac-address))

(define (checkout-mac-address section-name) 
  (fi-get-bmc-lan-conf-mac-address)) 

(define (commit-subnet-mask section-name subnet-mask)
  (fi-set-bmc-lan-conf-subnet-mask subnet-mask))

(define (checkout-subnet-mask section-name) 
  (fi-get-bmc-lan-conf-subnet-mask)) 

(define (commit-default-gateway-ip-address section-name gateway-ip-address)
  (fi-set-bmc-lan-conf-default-gateway-ip-address gateway-ip-address))

(define (checkout-default-gateway-ip-address section-name) 
  (fi-get-bmc-lan-conf-default-gateway-ip-address)) 

(define (commit-default-gateway-mac-address section-name gateway-mac-address)
  (fi-set-bmc-lan-conf-default-gateway-mac-address gateway-mac-address))

(define (checkout-default-gateway-mac-address section-name) 
  (fi-get-bmc-lan-conf-default-gateway-mac-address)) 

(define (commit-backup-gateway-ip-address section-name gateway-ip-address)
  (fi-set-bmc-lan-conf-backup-gateway-ip-address gateway-ip-address))

(define (checkout-backup-gateway-ip-address section-name) 
  (fi-get-bmc-lan-conf-backup-gateway-ip-address)) 

(define (commit-backup-gateway-mac-address section-name gateway-mac-address)
  (fi-set-bmc-lan-conf-backup-gateway-mac-address gateway-mac-address))

(define (checkout-backup-gateway-mac-address section-name) 
  (fi-get-bmc-lan-conf-backup-gateway-mac-address)) 

(define lan-conf-keys-validator 
  '(("ip_address_source" 
     valid-ip-address-source? 
     get-ip-address-source 
     commit-ip-address-source 
     checkout-ip-address-source 
     get-ip-address-source-value-string)
    ("ip_address" 
     valid-ip-address? 
     get-string 
     commit-ip-address 
     checkout-ip-address 
     get-string)
    ("mac_address" 
     valid-mac-address? 
     get-string 
     commit-mac-address 
     checkout-mac-address 
     get-string)
    ("subnet_mask" 
     valid-ip-address? 
     get-string 
     commit-subnet-mask 
     checkout-subnet-mask 
     get-string)
    ("default_gateway_ip_address" 
     valid-ip-address? 
     get-string 
     commit-default-gateway-ip-address 
     checkout-default-gateway-ip-address 
     get-string)
    ("default_gateway_mac_address" 
     valid-mac-address? 
     get-string 
     commit-default-gateway-mac-address 
     checkout-default-gateway-mac-address 
     get-string)
    ("backup_gateway_ip_address" 
     valid-ip-address? 
     get-string 
     commit-backup-gateway-ip-address 
     checkout-backup-gateway-ip-address 
     get-string)
    ("backup_gateway_mac_address" 
     valid-mac-address? 
     get-string 
     commit-backup-gateway-mac-address 
     checkout-backup-gateway-mac-address 
     get-string)
))

