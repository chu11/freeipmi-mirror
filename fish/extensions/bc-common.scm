(use-modules (srfi srfi-13))

(define privilege-limit-values '(("callback"        1) 
				 ("user"            2) 
				 ("operator"        3) 
				 ("administrator"   4) 
				 ("oem_proprietary" 5) 
				 ("no_access"       #xF)))

(define channel-access-modes '(("disabled"         0)
			       ("pre_boot_only"    1)
			       ("always_available" 2)
			       ("shared"           3)))

(define ip-address-sources '(("Unspecified"  0)
			     ("Static"       1)
			     ("Use_DHCP"     2)
			     ("Use_BIOS"     3)
			     ("Use_Others"   4)))

(define connect-modes '(("Modem_Connect"  0)
			("Direct_Connect" 1)))

(define flow-controls '(("No_Flow_Control" 0)
			("RTS_CTS"         1)
			("XON_XOFF"        2)))

(define bit-rates '(("9600"    6)
		    ("19200"   7)
		    ("38400"   8)
		    ("57600"   9)
		    ("115200"  10)))

(define power-restore-policies '(("Off_State_AC_Apply"      0)
				 ("Restore_State_AC_Apply"  1)
				 ("On_State_AC_Apply"       2)))

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

(define (get-privilege-limit string-token)
  (letrec ((get-priv-limit
	    (lambda (priv-limit-list)
	      (if (null? priv-limit-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car priv-limit-list)))
		      (cadr (car priv-limit-list))
		      (get-priv-limit (cdr priv-limit-list)))))))
    (get-priv-limit privilege-limit-values)))

(define (get-channel-access-mode string-token)
  (letrec ((get-access-mode 
	    (lambda (access-mode-list)
	      (if (null? access-mode-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car access-mode-list)))
		      (cadr (car access-mode-list))
		      (get-access-mode (cdr access-mode-list)))))))
    (get-access-mode channel-access-modes)))

(define (get-ip-address-source string-token)
  (letrec ((get-ip-source 
	    (lambda (ip-address-source-list)
	      (if (null? ip-address-source-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car ip-address-source-list)))
		      (cadr (car ip-address-source-list))
		      (get-ip-source (cdr ip-address-source-list)))))))
    (get-ip-source ip-address-sources)))

(define (get-connect-mode string-token)
  (letrec ((get-cmode 
	    (lambda (connect-mode-list)
	      (if (null? connect-mode-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car connect-mode-list)))
		      (cadr (car connect-mode-list))
		      (get-cmode (cdr connect-mode-list)))))))
    (get-cmode connect-modes)))

(define (get-flow-control string-token)
  (letrec ((get-fc 
	    (lambda (flow-control-list)
	      (if (null? flow-control-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car flow-control-list)))
		      (cadr (car flow-control-list))
		      (get-fc (cdr flow-control-list)))))))
    (get-fc flow-controls)))

(define (get-bit-rate string-token)
  (letrec ((get-br 
	    (lambda (bit-rate-list)
	      (if (null? bit-rate-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car bit-rate-list)))
		      (cadr (car bit-rate-list))
		      (get-br (cdr bit-rate-list)))))))
    (get-br bit-rates)))

(define (get-power-restore-policy string-token)
  (letrec ((get-policy 
	    (lambda (power-restore-policy-list)
	      (if (null? power-restore-policy-list)
		  #f
		  (if (string-ci=? (string-downcase string-token) 
				   (car (car power-restore-policy-list)))
		      (cadr (car power-restore-policy-list))
		      (get-policy (cdr power-restore-policy-list)))))))
    (get-policy power-restore-policies)))

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

(define (valid-privilege-limit? str)
  (if (not (string? str))
      #f
      (letrec ((check-privilege-limit
		(lambda (priv-limit-list)
		  (if (null? priv-limit-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car priv-limit-list)))
			  #t
			  (check-privilege-limit (cdr priv-limit-list)))))))
	(check-privilege-limit privilege-limit-values))))

(define (valid-channel-access-mode? str)
  (if (not (string? str))
      #f
      (letrec ((check-access-mode 
		(lambda (access-mode-list)
		  (if (null? access-mode-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car access-mode-list)))
			  #t
			  (check-access-mode (cdr access-mode-list)))))))
	(check-access-mode channel-access-modes))))

(define (valid-ip-address-source? str)
  (if (not (string? str))
      #f
      (letrec ((check-ip-source 
		(lambda (ip-address-source-list)
		  (if (null? ip-address-source-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car ip-address-source-list)))
			  #t
			  (check-ip-source (cdr ip-address-source-list)))))))
	(check-ip-source ip-address-sources))))

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

(define (valid-connect-mode? str)
  (if (not (string? str))
      #f
      (letrec ((check-connect-mode 
		(lambda (connect-mode-list)
		  (if (null? connect-mode-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car connect-mode-list)))
			  #t
			  (check-connect-mode (cdr connect-mode-list)))))))
	(check-connect-mode connect-modes))))

(define (valid-flow-control? str)
  (if (not (string? str))
      #f
      (letrec ((check-flow-control 
		(lambda (flow-control-list)
		  (if (null? flow-control-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car flow-control-list)))
			  #t
			  (check-flow-control (cdr flow-control-list)))))))
	(check-flow-control flow-controls))))

(define (valid-bit-rate? str)
  (if (not (string? str))
      #f
      (letrec ((check-bit-rate 
		(lambda (bit-rate-list)
		  (if (null? bit-rate-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car bit-rate-list)))
			  #t
			  (check-bit-rate (cdr bit-rate-list)))))))
	(check-bit-rate bit-rates))))

(define (valid-power-restore-policy? str)
  (if (not (string? str))
      #f
      (letrec ((check-power-restore-policy 
		(lambda (power-restore-policy-list)
		  (if (null? power-restore-policy-list)
		      #f
		      (if (string-ci=? (string-downcase str) (car (car power-restore-policy-list)))
			  #t
			  (check-power-restore-policy (cdr power-restore-policy-list)))))))
	(check-power-restore-policy power-restore-policies))))

