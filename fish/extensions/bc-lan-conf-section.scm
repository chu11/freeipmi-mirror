;;; bc-lan-conf-section.scm: BMC configurator LAN Conf section procedures
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

(define (commit-ip-address-source section-name ip-address-source)
  (if (list? ip-address-source)
      #t 
      (fi-set-bmc-lan-conf-ip-address-source ip-address-source)))

(define (checkout-ip-address-source section-name) 
  (fi-get-bmc-lan-conf-ip-address-source)) 

(define (commit-ip-address section-name ip-address)
  (if (list? ip-address)
      #t 
      (fi-set-bmc-lan-conf-ip-address ip-address)))

(define (checkout-ip-address section-name) 
  (fi-get-bmc-lan-conf-ip-address)) 

(define (commit-mac-address section-name mac-address)
  (if (list? mac-address)
      #t 
      (fi-set-bmc-lan-conf-mac-address mac-address)))

(define (checkout-mac-address section-name) 
  (fi-get-bmc-lan-conf-mac-address)) 

(define (commit-subnet-mask section-name subnet-mask)
  (if (list? subnet-mask)
      #t 
      (fi-set-bmc-lan-conf-subnet-mask subnet-mask)))

(define (checkout-subnet-mask section-name) 
  (fi-get-bmc-lan-conf-subnet-mask)) 

(define (commit-default-gateway-ip-address section-name gateway-ip-address)
  (if (list? gateway-ip-address)
      #t 
      (fi-set-bmc-lan-conf-default-gateway-ip-address gateway-ip-address)))

(define (checkout-default-gateway-ip-address section-name) 
  (fi-get-bmc-lan-conf-default-gateway-ip-address)) 

(define (commit-default-gateway-mac-address section-name gateway-mac-address)
  (if (list? gateway-mac-address)
      #t 
      (fi-set-bmc-lan-conf-default-gateway-mac-address gateway-mac-address)))

(define (checkout-default-gateway-mac-address section-name) 
  (fi-get-bmc-lan-conf-default-gateway-mac-address)) 

(define (commit-backup-gateway-ip-address section-name gateway-ip-address)
  (if (list? gateway-ip-address)
      #t 
      (fi-set-bmc-lan-conf-backup-gateway-ip-address gateway-ip-address)))

(define (checkout-backup-gateway-ip-address section-name) 
  (fi-get-bmc-lan-conf-backup-gateway-ip-address)) 

(define (commit-backup-gateway-mac-address section-name gateway-mac-address)
  (if (list? gateway-mac-address)
      #t 
      (fi-set-bmc-lan-conf-backup-gateway-mac-address gateway-mac-address)))

(define (checkout-backup-gateway-mac-address section-name)
  (fi-get-bmc-lan-conf-backup-gateway-mac-address))

(define (checkout-vlan-id-enable section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-vlan-id)))
    (if (list? param-list) (list (car param-list)) #f)))

(define (commit-vlan-id-enable section-name vlan-id-enable) 
  (if (list? vlan-id-enable)
      #t
      (fi-set-bmc-lan-conf-vlan-id vlan-id-enable "dummy")))

(define (checkout-vlan-id section-name) 
  (let ((param-list (fi-get-bmc-lan-conf-vlan-id)))
    (if (list? param-list) (list (cadr param-list)) #f)))

(define (commit-vlan-id section-name vlan-id) 
  (if (list? vlan-id)
      #t
      (fi-set-bmc-lan-conf-vlan-id 0 vlan-id)))

(define (checkout-vlan-priority section-name) 
  (fi-get-bmc-lan-conf-vlan-priority)) 

(define (commit-vlan-priority section-name vlan-priority)
  (if (list? vlan-priority)
      #t
      (fi-set-bmc-lan-conf-vlan-priority vlan-priority)))

(define lan-conf-keys-validator 
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
    ("ip_address_source" 
     valid-ip-address-source? 
     get-ip-address-source 
     commit-ip-address-source 
     checkout-ip-address-source 
     get-ip-address-source-value-string
     same-string-ci?
     "Possible values: Unspecified/Static/Use_DHCP/Use_BIOS/Use_Others")
    ("ip_address" 
     valid-ip-address? 
     get-string 
     commit-ip-address 
     checkout-ip-address 
     get-string
     same-string-ci?
     "Give valid IP Address")
    ("mac_address" 
     valid-mac-address? 
     get-string 
     commit-mac-address 
     checkout-mac-address 
     get-string
     same-string-ci?
     "Give valid MAC Address")
    ("subnet_mask" 
     valid-ip-address? 
     get-string 
     commit-subnet-mask 
     checkout-subnet-mask 
     get-string
     same-string-ci?
     "Give valid Subnet mask")
    ("default_gateway_ip_address" 
     valid-ip-address? 
     get-string 
     commit-default-gateway-ip-address 
     checkout-default-gateway-ip-address 
     get-string
     same-string-ci?
     "Give valid IP Address")
    ("default_gateway_mac_address" 
     valid-mac-address? 
     get-string 
     commit-default-gateway-mac-address 
     checkout-default-gateway-mac-address 
     get-string
     same-string-ci?
     "Give valid MAC Address")
    ("backup_gateway_ip_address" 
     valid-ip-address? 
     get-string 
     commit-backup-gateway-ip-address 
     checkout-backup-gateway-ip-address 
     get-string
     same-string-ci?
     "Give valid IP Address")
    ("backup_gateway_mac_address" 
     valid-mac-address? 
     get-string 
     commit-backup-gateway-mac-address 
     checkout-backup-gateway-mac-address 
     get-string
     same-string-ci?
     "Give valid MAC Address")
    ("vlan_id_enable" 
     valid-boolean? 
     get-boolean
     commit-vlan-id-enable
     checkout-vlan-id-enable
     get-boolean-string
     same-string-ci?
     "Possible values: Yes/No")
    ("vlan_id" 
     valid-integer? 
     get-integer
     commit-vlan-id 
     checkout-vlan-id
     any->string
     same-string-ci?
     "Give valid number.")
    ("vlan_priority" 
     valid-integer? 
     get-integer
     commit-vlan-priority 
     checkout-vlan-priority
     any->string
     same-string-ci?
     "Give valid number.")
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

