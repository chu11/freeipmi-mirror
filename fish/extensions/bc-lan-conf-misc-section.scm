(define (commit-enable-gratuitous-arps section-name enable-gratuitous-arps)
  (fi-set-bmc-lan-conf-arp-control enable-gratuitous-arps 0))

(define (commit-enable-arp-response section-name enable-arp-response)
  (fi-set-bmc-lan-conf-arp-control 0 enable-arp-response))

(define (commit-gratuitous-arp-interval section-name gratuitous-arp-interval)
  (fi-set-bmc-lan-conf-gratuitous-arp gratuitous-arp-interval))

(define lan-conf-misc-keys-validator 
  '(("enable_gratuitous_arps" valid-boolean? get-boolean commit-enable-gratuitous-arps)
    ("enable_arp_response" valid-boolean? get-boolean commit-enable-arp-response)
    ("gratuitous_arp_interval" valid-integer? get-integer commit-gratuitous-arp-interval)))
