(use-modules (srfi srfi-13))

(define (assoc-vref alist value)
  (if (null? alist)
      #f
      (if (equal? value (cdr (car alist)))
	  (caar alist)
	  (assoc-vref (cdr alist) value))))

(define privilege-limit-values '(("callback"        . 1) 
				 ("user"            . 2) 
				 ("operator"        . 3) 
				 ("administrator"   . 4) 
				 ("oem_proprietary" . 5) 
				 ("no_access"       . #xF)))

(define (get-privilege-limit value-string)
  (assoc-ref privilege-limit-values (string-downcase value-string)))

(define (valid-privilege-limit? str)
  (pair? (assoc (string-downcase str) privilege-limit-values)))

(define (get-privilege-limit-value-string value)
  (string-capitalize (assoc-vref privilege-limit-values value)))

(define channel-access-modes '(("disabled"         . 0)
			       ("pre_boot_only"    . 1)
			       ("always_available" . 2)
			       ("shared"           . 3)))
(define (get-channel-access-mode value-string)
  (assoc-ref channel-access-modes (string-downcase value-string)))

(define (valid-channel-access-mode? str)
  (pair? (assoc (string-downcase str) channel-access-modes)))

(define (get-channel-access-mode-value-string value)
  (string-capitalize (assoc-vref channel-access-modes value)))

(define ip-address-sources '(("unspecified" . 0)
			     ("static"      . 1)
			     ("use_dhcp"    . 2)
			     ("use_bios"    . 3)
			     ("use_others"  . 4)))

(define (get-ip-address-source value-string)
  (assoc-ref ip-address-sources (string-downcase value-string)))

(define (valid-ip-address-source? str)
  (pair? (assoc (string-downcase str) ip-address-sources)))

(define (get-ip-address-source-value-string value)
  (string-capitalize (assoc-vref ip-address-sources value)))

(define connect-modes '(("modem_connect"  . 0)
			("direct_connect" . 1)))

(define (get-connect-mode value-string)
  (assoc-ref connect-modes (string-downcase value-string)))

(define (valid-connect-mode? str)
  (pair? (assoc (string-downcase str) connect-modes)))

(define (get-connect-mode-value-string value)
  (string-capitalize (assoc-vref connect-modes value)))

(define flow-controls '(("no_flow_control" . 0)
			("rts_cts"         . 1)
			("xon_xoff"        . 2)))

(define (get-flow-control value-string)
  (assoc-ref flow-controls (string-downcase value-string)))

(define (valid-flow-control? str)
  (pair? (assoc (string-downcase str) flow-controls)))

(define (get-flow-control-value-string value)
  (string-capitalize (assoc-vref flow-controls value)))

(define bit-rates '(("9600"   . 6)
		    ("19200"  . 7)
		    ("38400"  . 8)
		    ("57600"  . 9)
		    ("115200" . 10)))

(define (get-bit-rate value-string)
  (assoc-ref bit-rates (string-downcase value-string)))

(define (valid-bit-rate? str)
  (pair? (assoc (string-downcase str) bit-rates)))

(define (get-bit-rate-value-string value)
  (string-capitalize (assoc-vref bit-rates value)))

(define power-restore-policies '(("off_state_ac_apply"     . 0)
				 ("restore_state_ac_apply" . 1)
				 ("on_state_ac_apply"      . 2)))

(define (get-power-restore-policy value-string)
  (assoc-ref power-restore-policies (string-downcase value-string)))

(define (valid-power-restore-policy? str)
  (pair? (assoc (string-downcase str) power-restore-policies)))

(define (get-power-restore-policy-value-string value)
  (string-capitalize (assoc-vref power-restore-policies value)))

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
      (if (string-ci=? (string-downcase key) (caar validator-def))
	  (primitive-eval (cadr (car validator-def)))
	  (get-value-validator key (cdr validator-def)))))

(define (get-convertor-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (caddr (car key-def)))
	  (get-convertor-proc key (cdr key-def)))))

(define (get-commit-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (cadddr (car key-def)))
	  (get-commit-proc key (cdr key-def)))))

(define (get-checkout-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (car (cddddr (car key-def))))
	  (get-checkout-proc key (cdr key-def)))))

(define (get-value-convertor-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (cadr (cddddr (car key-def))))
	  (get-value-convertor-proc key (cdr key-def)))))

(define (get-diff-proc key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (primitive-eval (caddr (cddddr (car key-def))))
	  (get-diff-proc key (cdr key-def)))))

(define (get-doc-string key key-def)
  (if (null? key-def)
      #f
      (if (string-ci=? (string-downcase key) (caar key-def))
	  (cadddr (cddddr (car key-def)))
	  (get-doc-string key (cdr key-def)))))

(define (get-string str) str)

(define (get-boolean str)
  (string-ci=? str "yes"))

(define (get-integer str)
  (string->number str))

(define (get-boolean-string bool)
  (if bool "Yes" "No"))

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

(define (file-exists? filename)
  (catch 'system-error 
	 (lambda ()
	   (if (eq? (stat:type (stat filename)) 'regular)
	       #t 
	       (begin 
		 (display 
		  (string-append "error: " filename ": is not a regular file\n") 
		  (current-error-port))
		 #f)))
	 (lambda error-info 
	   (let ((errno (system-error-errno error-info)))
	     (display (string-append "error: " filename ": ") (current-error-port))
	     (display (strerror errno) (current-error-port))
	     (display "\n" (current-error-port))
	     #f))))

(define (make-section key-string value)
  (if (= (length (string-split key-string #\:)) 2)
      (let* ((section-name-key-list (string-split key-string #\:))
	     (section-data (list (car section-name-key-list))))
	(append section-data
		(list (string-append (cadr section-name-key-list) " " value))))
      #f))

(define (same-string? section-name string1 string2)
  (string=? string1 string2))

(define (same-string-ci? section-name string1 string2)
  (string-ci=? string1 string2))

