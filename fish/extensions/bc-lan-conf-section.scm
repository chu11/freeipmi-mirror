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
  '(("ip_address_source" valid-ip-address-source? get-ip-address-source commit-ip-address-source)
    ("ip_address" valid-ip-address? get-string commit-ip-address)
    ("mac_address" valid-mac-address? get-string commit-mac-address)
    ("subnet_mask" valid-ip-address? get-string commit-subnet-mask)
    ("default_gateway_ip_address" valid-ip-address? get-string commit-default-gateway-ip-address)
    ("default_gateway_mac_address" valid-mac-address? get-string commit-default-gateway-mac-address)
    ("backup_gateway_ip_address" valid-ip-address? get-string commit-backup-gateway-ip-address)
    ("backup_gateway_mac_address" valid-mac-address? get-string commit-backup-gateway-mac-address)))

