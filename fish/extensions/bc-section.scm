(define section-name-desc '(("user1"          . user-keys-validator)
			    ("user2"          . user-keys-validator)
			    ("user3"          . user-keys-validator)
			    ("user4"          . user-keys-validator)
			    ("lan_channel"    . lan-serial-channel-keys-validator)
			    ("lan_conf"       . lan-conf-keys-validator)
			    ("lan_conf_auth"  . lan-conf-auth-keys-validator)
			    ("lan_conf_misc"  . lan-conf-misc-keys-validator)
			    ("serial_channel" . lan-serial-channel-keys-validator)
			    ("serial_conf"    . serial-conf-keys-validator)
			    ("misc"           . misc-keys-validator)))

(define (read-section-data fd)
  (letrec ((section-data '())
	   (line "")
	   (append-section-data 
	    (lambda ()
	      (set! line (read-valid-line fd))
	      (if (eof-object? line)
		  #f
		  (cond 
		   ((string-ci=? (car (string-tokenize line)) "section")
		    #f)
		   ((string-ci=? line "endsection")
		    section-data)
		   (else 
		    (set! section-data (append section-data 
					       (list (list->sentence 
						      (string-tokenize line)))))
		    (append-section-data)))))))
    (append-section-data)))

(define (read-section fd)
  (letrec ((section-name "")
	   (section-data "")
	   (line "")
	   (read-data 
	    (lambda ()
	      (set! line (read-valid-line fd))
	      (if (eof-object? line)
		  '()
		  (if (string-ci=? (car (string-tokenize line)) "section")
		      (begin 
			(set! section-name (cadr (string-tokenize line)))
			(set! section-data (read-section-data fd))
			(if (list? section-data)
			    (cons section-name section-data)
			    #f))
		      #f)))))
    (read-data)))

(define (validate-section-keys section-data section-keys)
  (let ((line "")
	(key "")
	(value "")
	(value-validator ""))
    (if (null? section-data)
	#t
	(begin 
	  (set! line (car section-data))
	  (set! key (car (string-tokenize line)))
	  (set! value (cadr (string-tokenize line)))
	  (set! value-validator (get-value-validator key section-keys)) 
	  (if (boolean? value-validator)
	      (begin 
		(display "ERROR: invalid key <") (display key) (display ">\n")
		#f)
	      (if (eval (list value-validator value) (interaction-environment))
		  (validate-section-keys (cdr section-data) section-keys)
		  (begin 
		    (display "ERROR: invalid value <") (display value) 
		    (display "> of key <") (display key) (display ">\n")
		    #f)))))))

(define (commit-section-values section-name section-data section-keys)
  (let ((line "")
	(key "")
	(value "")
	(convertor-proc "")
	(commit-proc ""))
    (if (null? section-data)
	#t
	(begin 
	  (set! line (car section-data))
	  (set! key (car (string-tokenize line)))
	  (set! value (cadr (string-tokenize line)))
	  (set! convertor-proc (get-convertor-proc key section-keys)) 
	  (set! commit-proc (get-commit-proc key section-keys))
	  (set! value (eval (list convertor-proc value) (interaction-environment)))
; 	  (display key) (newline)
; 	  (display value) (newline)
; 	  (display convertor-proc) (newline)
; 	  (display commit-proc) (newline)
	  (if (eval (list commit-proc section-name value) (interaction-environment))
	      (begin 
		(display (string-append "Function <" (simple->string commit-proc)
					"> Key <" key 
					"> Value <" (simple->string value) 
					"> DONE\n"))
		(commit-section-values section-name (cdr section-data) section-keys))
	      (begin 
		(display (string-append "Function <" (simple->string commit-proc) 
					"> Key <" key 
					"> Value <" (simple->string value) 
					"> FAILED\n"))
		#f))))))

(define (checkout-section-values section-name checkout-keys section-keys)
  (let ((key "")
	(value "")
	(checkout-proc "")
	(convertor-proc ""))
    (if (null? checkout-keys)
	#t
	(begin 
	  (set! key (car checkout-keys))
	  (set! checkout-proc (get-checkout-proc key section-keys))
	  (set! convertor-proc (get-value-convertor-proc key section-keys))
; 	  (display key) (newline)
; 	  (display convertor-proc) (newline)
; 	  (display checkout-proc) (newline)
	  (set! value ((primitive-eval checkout-proc) section-name))
; 	  (display value) (newline)
	  (if (list? value)
	      (begin 
		(set! value ((primitive-eval convertor-proc) (car value)))
		(display (string-append "Function <" (simple->string checkout-proc)
					"> Key <" key 
					"> Value <" value 
					"> DONE\n"))
		(checkout-section-values section-name (cdr checkout-keys) section-keys))
	      (begin 
		(display (string-append "Function <" (simple->string checkout-proc) 
					"> Key <" key 
					"> Value <" (simple->string value)
					"> FAILED\n"))
		#f))))))

(define (commit-section section-data)
  (let ((section-name (car section-data)))
    (commit-section-values section-name 
			   (cdr section-data)
			   (primitive-eval (assoc-ref section-name-desc 
						      (string-downcase section-name))))))

(define (checkout-section section-data)
  (let ((section-name (car section-data)))
    (checkout-section-values section-name 
			     (cdr section-data)
			     (primitive-eval (assoc-ref section-name-desc 
							(string-downcase section-name))))))

(define (validate-section section-data)
  (let ((section-name ""))
    (if (null? section-data)
	(begin 
	  (display "ERROR: invalid section found\n")
	  #f)
	(begin 
	  (set! section-name (car section-data))
	  (if (boolean? (assoc (string-downcase section-name) section-name-desc))
	      (begin 
		(display (string-append "ERROR: invalid section name <" 
					section-name ">\n"))
		#f)
	      (validate-section-keys (cdr section-data) 
				     (primitive-eval 
				      (assoc-ref section-name-desc 
						 (string-downcase section-name)))))))))
