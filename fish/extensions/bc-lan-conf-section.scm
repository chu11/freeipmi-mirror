(define (commit-ip-address-source section-name ip-address-source)
  (fi-set-bmc-lan-conf-ip-address-source ip-address-source))

(define (commit-ip-address section-name ip-address)
  (fi-set-bmc-lan-conf-ip-address ip-address))

(define (commit-mac-address section-name mac-address)
  (fi-set-bmc-lan-conf-mac-address mac-address))

(define (commit-subnet-mask section-name subnet-mask)
  (fi-set-bmc-lan-conf-subnet-mask subnet-mask))

(define (commit-default-gateway-ip-address section-name gateway-ip-address)
  (fi-set-bmc-lan-conf-default-gateway-ip-address gateway-ip-address))

(define (commit-default-gateway-mac-address section-name gateway-mac-address)
  (fi-set-bmc-lan-conf-default-gateway-mac-address gateway-mac-address))

(define (commit-backup-gateway-ip-address section-name gateway-ip-address)
  (fi-set-bmc-lan-conf-backup-gateway-ip-address gateway-ip-address))

(define (commit-backup-gateway-mac-address section-name gateway-mac-address)
  (fi-set-bmc-lan-conf-backup-gateway-mac-address gateway-mac-address))

(define lan-conf-keys-validator 
  '(("IP_Address_Source" valid-ip-address-source? get-ip-address-source commit-ip-address-source)
    ("IP_Address" valid-ip-address? get-string commit-ip-address)
    ("MAC_Address" valid-mac-address? get-string commit-mac-address)
    ("Subnet_Mask" valid-ip-address? get-string commit-subnet-mask)
    ("Default_Gateway_IP_Address" valid-ip-address? get-string commit-default-gateway-ip-address)
    ("Default_Gateway_MAC_Address" valid-mac-address? get-string commit-default-gateway-mac-address)
    ("Backup_Gateway_IP_Address" valid-ip-address? get-string commit-backup-gateway-ip-address)
    ("Backup_Gateway_MAC_Address" valid-mac-address? get-string commit-backup-gateway-mac-address)))

