(use-modules (srfi srfi-13))

(define privilege-limit-values '(("callback"        . 1) 
				 ("user"            . 2) 
				 ("operator"        . 3) 
				 ("administrator"   . 4) 
				 ("oem_proprietary" . 5) 
				 ("no_access"       . #xF)))

(define (get-privilege-limit string-token)
  (assoc-ref privilege-limit-values (string-downcase string-token)))

(define (valid-privilege-limit? str)
  (pair? (assoc (string-downcase str) privilege-limit-values)))

(define channel-access-modes '(("disabled"         . 0)
			       ("pre_boot_only"    . 1)
			       ("always_available" . 2)
			       ("shared"           . 3)))
(define (get-channel-access-mode string-token)
  (assoc-ref channel-access-modes (string-downcase string-token)))

(define (valid-channel-access-mode? str)
  (pair? (assoc (string-downcase str) channel-access-modes)))

(define ip-address-sources '(("unspecified" . 0)
			     ("static"      . 1)
			     ("use_dhcp"    . 2)
			     ("use_bios"    . 3)
			     ("use_others"  . 4)))

(define (get-ip-address-source string-token)
  (assoc-ref ip-address-sources (string-downcase string-token)))

(define (valid-ip-address-source? str)
  (pair? (assoc (string-downcase str) ip-address-sources)))

(define connect-modes '(("modem_connect"  . 0)
			("direct_connect" . 1)))

(define (get-connect-mode string-token)
  (assoc-ref connect-modes (string-downcase string-token)))

(define (valid-connect-mode? str)
  (pair? (assoc (string-downcase str) connect-modes)))

(define flow-controls '(("no_flow_control" . 0)
			("rts_cts"         . 1)
			("xon_xoff"        . 2)))

(define (get-flow-control string-token)
  (assoc-ref flow-controls (string-downcase string-token)))

(define (valid-flow-control? str)
  (pair? (assoc (string-downcase str) flow-controls)))

(define bit-rates '(("9600"   . 6)
		    ("19200"  . 7)
		    ("38400"  . 8)
		    ("57600"  . 9)
		    ("115200" . 10)))

(define (get-bit-rate string-token)
  (assoc-ref bit-rates (string-downcase string-token)))

(define (valid-bit-rate? str)
  (pair? (assoc (string-downcase str) bit-rates)))

(define power-restore-policies '(("off_state_ac_apply"     . 0)
				 ("restore_state_ac_apply" . 1)
				 ("on_state_ac_apply"      . 2)))

(define (get-power-restore-policy string-token)
  (assoc-ref power-restore-policies (string-downcase string-token)))

(define (valid-power-restore-policy? str)
  (pair? (assoc (string-downcase str) power-restore-policies)))

(define (read-valid-line fd)
  (let ((line (read-line fd)))
    (cond 
     ((eof-object? line)
      line)
     ((blank-line? line)
      (read-valid-line fd))
     ((comment-line? line)
      (read-valid-line fd))
     (else 
      (string-trim-both (car (string-split (car (string-split line #\#)) #\;)))))))

(define (get-value-validator key validator-def)
  (if (null? validator-def)
      #f
      (if (string-ci=? (string-downcase key) (car (car validator-def)))
	  (cadr (car validator-def))
	  (get-value-validator key (cdr validator-def)))))

(define (get-convertor-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (car (car key-def)))
	  (caddr (car key-def))
	  (get-convertor-proc key (cdr key-def)))))

(define (get-commit-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (car (car key-def)))
	  (cadddr (car key-def))
	  (get-commit-proc key (cdr key-def)))))

(define (get-string string-token)
  string-token)

(define (get-boolean string-token)
  (string-ci=? string-token "yes"))

(define (get-integer string-token)
  (string->number string-token))

(define (valid-username-password? str)
  (if (string? str)
      (<= (string-length str) 16)
      #f))

(define (valid-boolean? str)
  (if (string? str)
      (or (string-ci=? str "yes") (string-ci=? str "no"))
      #f))

(define (valid-integer? str)
  (if (string? str)
      (integer? (string->number str))
      #f))

(define (valid-ip-address? ip-address)
  (catch #t
	 (lambda ()
	   (inet-aton ip-address)
	   #t)
	 (lambda (k args . opts)
	   #f)))

(define (valid-mac-address? mac-address)
  (and (= (length (string-split mac-address #\:)) 6)
       (boolean? (member #f (map (lambda (s) (string->number s 16)) 
				 (string-split mac-address #\:))))))
